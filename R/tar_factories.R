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
            read_archived(tar_name_restore, store = .(path_restore))
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
            \(x) as.call(c(as.symbol("list"), x))
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

tar_clean_indices <- function(tar_name_indices = "indices",
                              id_cols = "user_id") {
  tar_name_indices_clean <- paste0(tar_name_indices, "_clean")
  tar_name_indices_of_interest <- paste0(tar_name_indices, "_of_interest")
  tar_name_indices_wider_clean <- paste0(tar_name_indices, "_wider_clean")
  list(
    tar_target_raw(
      tar_name_indices_clean,
      bquote(
        .(as.symbol(tar_name_indices)) |>
          semi_join(users_completed, by = "user_id") |>
          # https://github.com/r-lib/vctrs/issues/1787
          arrange(desc(game_time)) |>
          distinct(pick(.(id_cols)), game_id, index_name, .keep_all = TRUE) |>
          left_join(
            res_motivated,
            by = setdiff(colnames(res_motivated), "is_motivated")
          )
      )
    ),
    tar_target_raw(
      tar_name_indices_of_interest,
      bquote(
        .(as.symbol(tar_name_indices_clean)) |>
          inner_join(
            data.iquizoo::game_indices,
            join_by(game_id, index_name == index_main)
          ) |>
          mutate(score_adj = if_else(index_reverse, -score, score)) |>
          mutate(
            is_outlier_iqr = score %in% boxplot.stats(score)$out,
            .by = c(game_id, index_name)
          )
      )
    ),
    tar_target_raw(
      tar_name_indices_wider_clean,
      bquote(
        .(as.symbol(tar_name_indices_of_interest)) |>
          filter(
            !is_outlier_iqr & is_motivated,
            # RAPM test is not included in the factor analysis
            game_id != game_id_rapm
          ) |>
          left_join(data.iquizoo::game_info, by = "game_id") |>
          mutate(game_index = str_c(game_name_abbr, index_name, sep = ".")) |>
          pivot_wider(
            id_cols = .(id_cols),
            names_from = game_index,
            values_from = score_adj
          )
      )
    )
  )
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
tar_fit_cfa <- function(config, data, theory, col_latent, col_manifest,
                        col_fix = NULL, add_gof = TRUE, add_scores = TRUE) {
  list(
    tar_target_raw(
      "fit",
      substitute(
        fit_cfa(
          config,
          data,
          theory,
          col_manifest = col_manifest,
          col_latent = col_latent,
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

tar_sample_tasks <- function(num_tasks, data,
                             name_id_col = 1,
                             sample_times = 10,
                             sample_size = 100,
                             name_suffix = "") {
  cfg_rsmp_vars <- withr::with_seed(
    1,
    tidyr::expand_grid(
      num_vars = round(seq(3, floor(num_tasks / 2), length.out = sample_times)),
      idx_rsmp = seq_len(sample_size)
    ) |>
      dplyr::reframe(
        purrr::map(
          num_vars,
          ~ data.frame(
            id_pairs = rep(c(1, 2), .),
            idx_vars = sample.int(num_tasks, . * 2, replace = FALSE)
          )
        ) |>
          purrr::list_rbind(),
        .by = c(num_vars, idx_rsmp)
      ) |>
      tidyr::chop(idx_vars) |>
      tidyr::chop(c(idx_rsmp, idx_vars))
  )
  tarchetypes::tar_map(
    values = cfg_rsmp_vars,
    names = c(num_vars, id_pairs),
    tar_target_raw(
      paste0("data_names", name_suffix),
      tibble(
        idx_rsmp = idx_rsmp, # use this to track samples
        tasks = map(idx_vars, ~ names(data)[-name_id_col][.])
      ) |>
        substitute(),
      deployment = "main"
    ),
    tar_target_raw(
      paste0("mdl_fitted", name_suffix),
      .(as.name(paste0("data_names", name_suffix))) |>
        mutate(
          mdl = map(tasks, ~ fit_g(.(substitute(data)), all_of(.))),
          .keep = "unused"
        ) |>
        bquote()
    ),
    tar_target_raw(
      paste0("scores_g", name_suffix),
      .(as.name(paste0("mdl_fitted", name_suffix))) |>
        mutate(
          scores = map(mdl, ~ predict_g_score(.(substitute(data)), .)),
          .keep = "unused"
        ) |>
        bquote()
    )
  )
}

# general targets factory ----
tar_prep_creativity <- function() {
  list(
    tarchetypes::tar_file_read(
      cr_correction,
      "data/cr_correction.parquet",
      read = arrow::read_parquet(!!.x)
    ),
    tarchetypes::tar_file_read(
      aut_grade_scores,
      "data/aut_grade_scores.parquet",
      read = arrow::read_parquet(!!.x)
    ),
    tarchetypes::tar_file_read(
      aut_grade_types,
      "data/aut_grade_types.parquet",
      read = arrow::read_parquet(!!.x)
    ),
    tarchetypes::tar_file_read(
      vg_dists,
      "data/vg_dists.parquet",
      read = arrow::read_parquet(!!.x)
    ),
    tarchetypes::tar_file_read(
      dat_w2v,
      "data/dat_w2v.qs",
      read = qs::qread(!!.x)
    )
  )
}
