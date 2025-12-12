# data wrangling ----
tar_collect_camp <- function(contents) {
  path_archive <- Sys.getenv("OneDriveConsumer") |>
    fs::path("Documents/Research/archived/cogstruct-dev-archived")
  path_restore <- withr::with_dir(
    path_archive,
    fs::path(
      path_archive,
      tar_config_get("store", project = "preproc_behav")
    )
  )
  config_contents <- contents |>
    dplyr::distinct(game_id) |>
    dplyr::inner_join(data.iquizoo::game_info, by = "game_id") |>
    dplyr::mutate(
      tar_name_current = rlang::syms(sprintf("raw_data_%s", game_id)),
      tar_name_restore = rlang::syms(sprintf("data_%s", game_name_abbr)),
      tar_name_data_full = rlang::syms(sprintf("data_full_%s", game_id)),
      tar_name_data_parsed = rlang::syms(sprintf("raw_data_parsed_%s", game_id))
    )
  list(
    data_full = tarchetypes::tar_eval_raw(
      bquote(
        tar_target(
          tar_name_data_full,
          bind_rows(
            select(tar_name_current, -project_id),
            zutils::cautiously(targets::tar_read, tibble())(
              tar_name_restore,
              store = .(path_restore)
            ) |>
              select(!contains("name"))
          ) |>
            distinct()
        )
      ),
      config_contents
    ),
    raw_data_parsed = tarchetypes::tar_eval(
      tar_target(
        tar_name_data_parsed,
        parse_data(tar_name_data_full),
        packages = "tarflow.iquizoo"
      ),
      config_contents
    )
  )
}

tar_validate_device <- function(contents) {
  tarchetypes::tar_map(
    contents |>
      dplyr::distinct(game_id) |>
      dplyr::filter(game_id %in% game_id_keyboard) |>
      dplyr::mutate(
        data_raw = rlang::syms(sprintf("raw_data_parsed_%s", game_id)),
        game_id = as.character(game_id)
      ),
    names = game_id,
    tar_target(
      device_validity,
      data_raw |>
        mutate(
          device_valid = map_lgl(
            raw_parsed,
            check_device
          ),
          .keep = "unused"
        )
    )
  )
}

tar_validate_rawdata <- function(contents) {
  prepare_command_correction <- function(correction, game_id) {
    expr <- as.symbol(sprintf("raw_data_parsed_%s", game_id))
    if (!is.na(correction)) {
      expr <- zutils::call_full(
        sprintf("correct_%s", correction),
        data = !!expr
      )
    }
    expr
  }
  contents |>
    dplyr::distinct(game_id) |>
    dplyr::left_join(game_versions, by = "game_id") |>
    dplyr::left_join(game_data_names, by = "game_id") |>
    dplyr::left_join(config_data_correction, by = "game_id") |>
    dplyr::mutate(
      require_keyboard = game_id %in% game_id_keyboard,
      tar_name_data_valid = rlang::syms(sprintf("data_valid_%s", game_id))
    ) |>
    tidyr::nest(.by = c(game_id, correction)) |>
    purrr::pmap(
      \(correction, game_id, data) {
        tarchetypes::tar_eval_raw(
          bquote(
            tar_target(
              tar_name_data_valid,
              .(prepare_command_correction(correction, game_id)) |>
                validate_data(
                  list_versions = game_versions,
                  require_keyboard = require_keyboard,
                  list_names = list_names
                )
            )
          ),
          data
        )
      }
    )
}

tar_check_motivated <- function(config) {
  config_branches <- config |>
    dplyr::filter(!is.na(rule)) |>
    tidyr::separate_longer_delim(rule, ";") |>
    dplyr::mutate(
      tar_name_data_valid = rlang::syms(
        sprintf("data_valid_%s", game_id)
      ),
      tar_name_motivated = rlang::syms(
        sprintf("res_motivated_%s_%s", rule, game_id)
      )
    )
  list(
    purrr::imap(
      split(config_branches, ~rule),
      function(config, rule) {
        tarchetypes::tar_eval_raw(
          bquote(
            tar_target(
              tar_name_motivated,
              tar_name_data_valid |>
                mutate(
                  is_motivated = map_lgl(
                    raw_parsed,
                    \(data) .(zutils::call_full(sprintf("check_%s", rule)))
                  ),
                  .keep = "unused"
                )
            )
          ),
          config
        )
      }
    ),
    res_motivated = tarchetypes::tar_eval(
      tar_target(
        tar_name_motivated_final,
        bind_rows(tar_name_motivated_list) |>
          summarise(
            is_motivated = all(is_motivated),
            .by = !is_motivated
          )
      ),
      config_branches |>
        dplyr::select(game_id, tar_name_motivated) |>
        tidyr::chop(tar_name_motivated) |>
        dplyr::mutate(
          tar_name_motivated_list = lapply(
            tar_name_motivated,
            \(x) as.call(c(quote(list), x))
          ),
          tar_name_motivated_final = rlang::syms(
            sprintf("res_motivated_%s", game_id)
          )
        )
    )
  )
}

tar_partition_rawdata <- function(contents) {
  contents |>
    dplyr::distinct(game_id) |>
    data.iquizoo::merge_preproc() |>
    dplyr::inner_join(config_format, by = "game_id") |>
    dplyr::filter(!is.na(format)) |>
    dplyr::mutate(
      data = rlang::syms(sprintf("data_valid_%s", game_id)),
      tar_name_indices = rlang::syms(
        sprintf("indices_slices_%s", game_id)
      ),
      prep_fun = dplyr::if_else(
        # 社交达人 needs a new prep fun for sliced versions of data
        game_id == "381576542159749",
        rlang::syms("fname_slices"),
        prep_fun
      )
    ) |>
    split(~format) |>
    purrr::imap(
      function(config, format) {
        tarchetypes::tar_eval_raw(
          bquote(
            tar_target(
              tar_name_indices,
              .(zutils::call_full(sprintf("slice_data_%s", format))) |>
                preproc_data(prep_fun, .input = input, .extra = extra)
            )
          ),
          config
        )
      }
    )
}

# item analysis related ----
tar_test_retest <- function(
  contents,
  ...,
  cols_by = c("ver_major", "index_name"),
  add_compare = TRUE,
  name_suffix = NULL,
  extra_by = NULL
) {
  rlang::check_dots_empty()
  suffix <- if (is.null(name_suffix)) "" else paste0("_", name_suffix)
  chr <- function(x, ...) paste(paste0(x, suffix), ..., sep = "_")
  sym <- function(x, ...) as.symbol(chr(x, ...))
  values <- prepare_config_retest(contents)
  list(
    indices_retest = purrr::pmap(
      values,
      \(game_id_real, game_id) {
        tar_target_raw(
          chr("indices_retest", game_id_real),
          bquote(
            clean_retest(
              bind_rows(..(rlang::syms(chr("indices", game_id)))),
              .(extra_by)
            ),
            splice = TRUE
          )
        )
      }
    ),
    test_retest = purrr::pmap(
      values,
      \(game_id_real, ...) {
        tar_target_raw(
          chr("test_retest", game_id_real),
          bquote(
            if (!is.null(.(sym("indices_retest", game_id_real)))) {
              # https://github.com/tidyverse/dplyr/issues/7008
              # `reframe()` is preferable but there is some issue
              .(sym("indices_retest", game_id_real)) |>
                group_by(pick(.(c(cols_by, extra_by)))) |>
                group_modify(calc_test_retest) |>
                ungroup()
            }
          )
        )
      }
    ),
    compare_retest = if (add_compare) {
      purrr::pmap(
        values,
        \(game_id_real, ...) {
          tar_target_raw(
            chr("compare_retest", game_id_real),
            bquote(
              if (!is.null(.(sym("indices_retest", game_id_real)))) {
                .(sym("indices_retest", game_id_real)) |>
                  summarise(
                    t.test(test, retest, paired = TRUE) |>
                      broom::tidy(),
                    .by = .(cols_by)
                  )
              }
            )
          )
        }
      )
    }
  )
}

# modeling related ----
tar_fit_cfa <- function(
  config,
  data,
  theory,
  col_ov = manifest,
  col_lv = latent,
  col_fix = NULL,
  missing = "ml",
  tar_post_fit = c("gof", "comp_rel", "scores")
) {
  tar_post_fit <- match.arg(tar_post_fit, several.ok = TRUE)
  list(
    tar_target_raw(
      "fit",
      substitute(
        fit_cfa(
          config,
          data,
          theory,
          missing = missing,
          col_ov = col_ov,
          col_lv = col_lv,
          col_fix = col_fix
        )
      )
    ),
    if ("gof" %in% tar_post_fit) {
      tar_target_raw(
        "gof",
        quote(
          if (inherits(fit, "lavaan") && lavInspect(fit, "converged")) {
            as_tibble_row(unclass(fitmeasures(fit)))
          }
        )
      )
    },
    if ("comp_rel" %in% tar_post_fit) {
      tar_target_raw(
        "comp_rel",
        quote(
          if (inherits(fit, "lavaan") && lavInspect(fit, "converged")) {
            semTools::compRelSEM(fit)
          }
        )
      )
    },
    if ("scores" %in% tar_post_fit) {
      tar_target_raw(
        "scores",
        substitute(
          if (inherits(fit, "lavaan") && lavInspect(fit, "converged")) {
            extract_latent_scores(fit, data)
          }
        )
      )
    }
  )
}

tar_prepare_neural_data <- function(config) {
  c(
    tarchetypes::tar_eval_raw(
      bquote(
        tar_target(
          file_fc,
          path_obj_from_proj(
            .(
              as.call(c(
                quote(paste),
                "fc",
                rlang::syms(names(config_fc)),
                sep = "_"
              ))
            ),
            "prepare_neural"
          ),
          format = "file_fast"
        )
      ),
      config
    ),
    tar_target(
      file_fd,
      path_obj_from_proj("fd_mean", "prepare_neural"),
      format = "file_fast"
    ),
    purrr::pmap(
      dplyr::distinct(config, session, task, run, fd),
      \(session, task, run, fd) {
        if (task == "latent") {
          if (run == "full") {
            cols <- seq_len(nrow(params_fmri_tasks))
          } else {
            cols <- which(params_fmri_tasks$run_id %in% parse_digits(run))
          }
        } else {
          if (session == "0") {
            cols <- which(params_fmri_tasks$task == task)
          } else {
            if (run == "full") {
              cols <- which(
                params_fmri_tasks$session == session &
                  params_fmri_tasks$task == task
              )
            } else {
              cols <- which(
                params_fmri_tasks$session == session &
                  params_fmri_tasks$task == task &
                  params_fmri_tasks$run_id %in% parse_digits(run)
              )
            }
          }
        }
        eval(substitute(
          tar_target(
            fd,
            as.matrix(rowMeans(qs::qread(file_fd)[, cols, drop = FALSE]))
          ),
          list(fd = fd, cols = cols)
        ))
      }
    ),
    tarchetypes::tar_eval(
      tar_target(
        file_atlas_dseg,
        path_obj_from_proj(sprintf("atlas_dseg_%s", atlas), "prepare_neural"),
        format = "file_fast"
      ),
      dplyr::distinct(config, atlas, file_atlas_dseg)
    ),
    # commonly used targets
    tarchetypes::tar_file_read(
      users_confounds,
      path_obj_from_proj("users_confounds", "prepare_source_data"),
      read = qs::qread(!!.x)
    ),
    tarchetypes::tar_file_read(
      subjs_keep_neural,
      path_obj_from_proj("subjs_keep_neural", "prepare_neural"),
      read = qs::qread(!!.x)
    )
  )
}

# g factor ----
tar_calibrate_g <- function(
  expr,
  data,
  use_pairs,
  ...,
  name_suffix = NULL,
  data_crit = NULL,
  config_neural = NULL,
  hypers_cpm = NULL
) {
  suffix <- if (is.null(name_suffix)) "" else paste0("_", name_suffix)
  chr <- function(x) paste0(x, suffix)
  sym <- function(x) as.symbol(chr(x))
  c(
    tarchetypes::tar_rep_raw(
      chr("vars_sample"),
      substitute(expr),
      ...,
      iteration = "list",
      deployment = "main"
    ),
    tarchetypes::tar_rep2_raw(
      chr("fit_g"),
      bquote(
        lapply_tar_batches(
          .(sym("vars_sample")),
          fit_efa_g,
          data = .(substitute(data)),
          missing = "ml"
        )
      ),
      chr("vars_sample"),
      iteration = "list"
    ),
    tarchetypes::tar_rep2_raw(
      chr("comp_rel_g"),
      bquote(
        lapply_tar_batches(
          .(sym("fit_g")),
          \(x) tibble(comp_rel = unclass(semTools::compRelSEM(x$nf1)))
        ) |>
          list_rbind_tar_batches(names_to = "id_pairs")
      ),
      chr("fit_g")
    ),
    tarchetypes::tar_rep2_raw(
      chr("scores_g"),
      bquote(
        lapply_tar_batches(
          .(sym("fit_g")),
          extract_g_scores,
          data = .(substitute(data))
        )
      ),
      chr("fit_g"),
      iteration = "list"
    ),
    tarchetypes::tar_rep2_raw(
      chr("rel_pairs_g"),
      bquote(
        tibble(
          r = if (.(substitute(use_pairs))) {
            as.vector(
              cor(
                .(sym("scores_g"))[[1]],
                .(sym("scores_g"))[[2]],
                use = "pairwise"
              )
            )
          }
        )
      ),
      chr("scores_g")
    ),
    if (!is.null(substitute(data_crit))) {
      purrr::imap(
        # https://stackoverflow.com/q/52392301/5996475
        as.list(substitute(data_crit))[-1],
        \(obj, name) {
          tarchetypes::tar_rep2_raw(
            chr(name),
            bquote(
              lapply_tar_batches(
                .(sym("scores_g")),
                \(x) {
                  subjs <- intersect(rownames(x), rownames(.(obj)))
                  tibble(r = cor(x[subjs, ], .(obj)[subjs, ], use = "pairwise"))
                }
              ) |>
                list_rbind_tar_batches(names_to = "id_pairs")
            ),
            chr("scores_g")
          )
        }
      )
    },
    if (!is.null(config_neural) && !is.null(hypers_cpm)) {
      tarchetypes::tar_map(
        tidyr::expand_grid(config_neural, hypers_cpm),
        names = !all_of(names_exclude),
        tarchetypes::tar_rep2_raw(
          chr("cpm_result"),
          bquote(
            lapply_tar_batches(
              .(sym("scores_g")),
              perform_cpm,
              fc = qs::qread(file_fc),
              confounds = match_confounds(users_confounds, fd),
              subjs_keep_neural = subjs_keep_neural,
              bias_correct = FALSE,
              thresh_method = thresh_method,
              thresh_level = thresh_level,
              return_edges = "sum"
            )
          ),
          chr("scores_g"),
          iteration = "list",
          retrieval = "worker",
          storage = "worker"
        ),
        tarchetypes::tar_rep2_raw(
          chr("dice_pairs"),
          bquote(
            if (.(substitute(use_pairs))) {
              calc_dice_pairs(.(sym("cpm_result")), 0.5)
            } else {
              tibble()
            }
          ),
          chr("cpm_result"),
          retrieval = "worker",
          storage = "worker"
        ),
        tarchetypes::tar_rep2_raw(
          chr("cpm_performance"),
          bquote(
            lapply_tar_batches(
              .(sym("cpm_result")),
              extract_cpm_performance
            ) |>
              list_rbind_tar_batches(names_to = "id_pairs")
          ),
          chr("cpm_result"),
          retrieval = "worker",
          storage = "worker"
        )
      )
    }
  )
}

tar_combine_branches <- function(
  name,
  branches,
  targets,
  meta_names,
  meta_prefix = name,
  names_greedy = NULL
) {
  rlang::check_exclusive(targets, branches)
  if (missing(targets)) {
    targets <- zutils::select_list(branches, starts_with(name))
  }
  tarchetypes::tar_combine_raw(
    name,
    targets,
    command = bquote(
      bind_rows_meta(
        !!!.x,
        .names = .(meta_names),
        .prefix = .(meta_prefix),
        .names_greedy = .(names_greedy)
      )
    ),
    deployment = "main"
  )
}
