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
              tar_name_restore, store = .(path_restore)
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
        wrangle_data(tar_name_data_full)
      ),
      config_contents
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
    data.iquizoo::match_preproc(type = "inner") |>
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
tar_test_retest <- function(contents, ...,
                            name_suffix = NULL,
                            extra_by = NULL) {
  rlang::check_dots_empty()
  tar_name_indices_retest <- paste(
    c("indices_retest", name_suffix),
    collapse = "_"
  )
  tar_name_test_retest <- paste(
    c("test_retest", name_suffix),
    collapse = "_"
  )
  tarchetypes::tar_map(
    prepare_config_retest(contents, name_suffix),
    names = game_id,
    tar_target_raw(
      tar_name_indices_retest,
      substitute(clean_retest(bind_rows(tar_indices), extra_by))
    ),
    tar_target_raw(
      tar_name_test_retest,
      bquote(
        if (!is.null(.(as.symbol(tar_name_indices_retest)))) {
          calc_test_retest(
            .(as.symbol(tar_name_indices_retest)),
            .(substitute(extra_by))
          )
        }
      )
    )
  )
}

# modeling related ----
tar_fit_cfa <- function(config, data, theory,
                        col_ov = manifest,
                        col_lv = latent,
                        col_fix = NULL,
                        missing = "ml",
                        tar_post_fit = c("gof", "comp_rel", "scores")) {
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

tar_prepare_cpm_data <- function(config) {
  c(
    tarchetypes::tar_eval_raw(
      bquote(
        tar_target(
          file_fc,
          path_obj_from_proj(
            .(
              as.call(c(quote(paste), "fc",
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
