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
    dplyr::left_join(game_data_names, by = "game_id") |>
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
  config_contents <- contents |>
    dplyr::distinct(game_id) |>
    dplyr::inner_join(data.iquizoo::game_info, by = "game_id") |>
    dplyr::left_join(game_data_names, by = "game_id") |>
    dplyr::mutate(
      game_id = as.character(game_id),
      require_keyboard = game_name %in% games_keyboard,
      tar_parsed = rlang::syms(sprintf("raw_data_parsed_%s", game_id))
    )
  c(
    tarchetypes::tar_map(
      values = config_contents |>
        dplyr::filter(!game_id %in% do.call(c, game_id_cor)),
      names = game_id,
      tar_target(
        data_valid,
        validate_data(
          tar_parsed,
          require_keyboard = require_keyboard,
          list_names = list_names
        )
      )
    ),
    # correct device error
    tarchetypes::tar_map(
      values = config_contents |>
        dplyr::filter(game_id %in% game_id_cor$dev_err),
      names = game_id,
      tar_target(
        data_valid,
        correct_device(tar_parsed) |>
          validate_data(
            require_keyboard = require_keyboard,
            list_names = list_names
          )
      )
    ),
    tarchetypes::tar_map(
      values = config_contents |>
        dplyr::filter(game_id %in% game_id_cor$dur_err),
      names = game_id,
      tar_target(
        data_valid,
        validate_data(
          tar_parsed,
          require_keyboard = require_keyboard,
          list_names = list_names
        ) |>
          correct_game_dur()
      )
    ),
    # correct accuracy scores for Category Retrieval (CR)
    if (FALSE) { # disabled because it requires more additional work
      tarchetypes::tar_map(
        values = config_contents |>
          dplyr::filter(game_id == game_id_cor$cr),
        names = game_id,
        tar_target(
          data_valid,
          validate_data(
            tar_parsed,
            require_keyboard = require_keyboard,
            list_names = list_names
          ) |>
            correct_cr(cr_correction)
        )
      )
    },
    # keep test phase only for Mnemonic Similarity Task (MST)
    tarchetypes::tar_map(
      values = config_contents |>
        dplyr::filter(game_id %in% game_id_cor$mst),
      names = game_id,
      tar_target(
        data_valid,
        validate_data(
          tar_parsed,
          require_keyboard = require_keyboard,
          list_names = list_names
        ) |>
          correct_mst()
      )
    ),
    # correct reaction time error for flanker test
    tarchetypes::tar_map(
      values = config_contents |>
        dplyr::filter(game_id %in% game_id_cor$rt) |>
        dplyr::mutate(
          adjust = dplyr::case_when(
            game_id == "224379118576069" ~ 100,
            game_id == "268008982667347" ~ 300
          )
        ),
      names = game_id,
      tar_target(
        data_valid,
        validate_data(
          tar_parsed,
          require_keyboard = require_keyboard,
          list_names = list_names
        ) |>
          correct_rt(adjust = adjust)
      )
    )
  )
}

tar_check_motivated <- function(config) {
  tar_check_motivated_ <- function(config, rule) {
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
      tar_check_motivated_
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

tar_partition_rawdata <- function(contents, config_format) {
  tar_partition_rawdata_ <- function(config, format) {
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
    purrr::imap(tar_partition_rawdata_)
}

# item analysis related ----
tar_test_retest <- function(contents, ...,
                            by = NULL,
                            name_indices = "indices",
                            name_test_retest = "test_retest") {
  rlang::check_dots_empty()
  tarchetypes::tar_map(
    values = contents |>
      dplyr::distinct(game_id) |>
      data.iquizoo::match_preproc(type = "semi", rm_tagged = TRUE) |>
      dplyr::left_join(data.iquizoo::game_info, by = "game_id") |>
      dplyr::mutate(
        game_id_rel = dplyr::coalesce(game_id_parallel, game_id)
      ) |>
      dplyr::summarise(
        tar_indices = rlang::syms(
          stringr::str_glue("{name_indices}_{game_id}")
        ) |>
          list(),
        .by = game_id_rel
      ) |>
      dplyr::mutate(game_id_rel = as.character(game_id_rel)),
    names = game_id_rel,
    if (is.null(by)) {
      tar_target_raw(
        name_test_retest,
        quote(calc_test_retest(list_rbind(tar_indices)))
      )
    } else {
      tar_target_raw(
        name_test_retest,
        bquote(
          list_rbind(tar_indices) |>
            group_by(pick(.(by))) |>
            group_modify(~ calc_test_retest(.x)) |>
            ungroup()
        )
      )
    }
  )
}

# modeling related ----
tar_fit_cfa <- function(config, data, theory,
                        col_ov = observed, col_lv = latent, col_fix = NULL,
                        add_gof = TRUE, add_scores = TRUE) {
  list(
    tar_target_raw(
      "fit",
      substitute(
        fit_cfa(
          config,
          data,
          theory,
          col_ov = col_ov,
          col_lv = col_lv,
          col_fix = col_fix
        )
      )
    ),
    if (add_gof) {
      tar_target_raw(
        "gof",
        quote(
          if (inherits(fit, "lavaan")) {
            as_tibble_row(unclass(fitmeasures(fit)))
          }
        )
      )
    },
    if (add_scores) {
      tar_target_raw(
        "scores",
        substitute(
          if (inherits(fit, "lavaan")) {
            extract_latent_scores(fit, data)
          }
        )
      )
    }
  )
}

tar_prep_files_cpm <- function(...) {
  values <- prepare_config_cpm(...)
  c(
    tarchetypes::tar_eval(
      tar_target(
        file_confounds,
        path_obj_from_proj(
          paste(
            "confounds_cpm",
            session, task,
            sep = "_"
          ),
          "preproc_neural"
        ),
        format = "file_fast"
      ),
      dplyr::distinct(values, session, task, file_confounds)
    ),
    tarchetypes::tar_eval(
      tar_target(
        file_fc,
        path_obj_from_proj(
          paste(
            "fc_orig_full",
            session, task, config, atlas,
            sep = "_"
          ),
          "preproc_neural"
        ),
        format = "file_fast"
      ),
      dplyr::distinct(values, session, task, config, atlas, file_fc)
    )
  )
}

tar_cpm_main <- function(command, ..., batches = 4, reps = 5, combine = TRUE) {
  config_cpm <- prepare_config_cpm(...)
  cpm_branches <- tarchetypes::tar_map(
    config_cpm,
    names = !starts_with("file"),
    tarchetypes::tar_rep_raw(
      "cpm_result",
      substitute(command),
      batches = batches,
      reps = reps,
      iteration = "list",
      retrieval = "worker",
      storage = "worker"
    ),
    tarchetypes::tar_rep2(
      cpm_performance,
      aggregate_performance(cpm_result),
      cpm_result,
      retrieval = "worker",
      storage = "worker"
    )
  )
  if (isTRUE(combine)) {
    combine <- names(cpm_branches[-c(1:2)])
  }
  c(
    cpm_branches,
    lapply(
      intersect(names(cpm_branches), combine),
      \(x) {
        tarchetypes::tar_combine_raw(
          x,
          cpm_branches[[x]],
          command = bquote(
            bind_rows(!!!.x, .id = ".id") |>
              zutils::separate_wider_dsv(
                ".id",
                .(names(dplyr::select(config_cpm, !starts_with("file")))),
                # xcpd used "_" in its config names
                patterns = c(rep(".+?", 2), ".+", rep(".+?", 3)),
                prefix = .(x)
              )
          )
        )
      }
    )
  )
}

prepare_config_cpm <- function(...) {
  tidyr::expand_grid(
    params_fmri_tasks,
    params_xcpd,
    hypers_cpm
  ) |>
    dplyr::filter(...) |>
    dplyr::mutate(
      file_fc = rlang::syms(
        sprintf(
          "file_fc_%s_%s_%s_%s",
          session, task, config, atlas
        )
      ),
      file_confounds = rlang::syms(
        sprintf(
          "file_confounds_%s_%s",
          session, task
        )
      )
    )
}
