targets_sample_tasks <- function(num_tasks, data,
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

combine_targets <- function(name, targets, cols_targets) {
  name <- deparse1(substitute(name))
  tarchetypes::tar_combine_raw(
    name,
    targets[[name]],
    command = bind_rows(!!!.x, .id = "id") |>
      # note there is delimiter after name should be removed too
      mutate(id = str_remove(id, str_c(name, "."))) |>
      separate(id, cols_targets, convert = TRUE) |>
      substitute()
  )
}

prepare_data <- function(games, name_config, path_restore,
                         name_suffix = "restore") {
  rlang::check_exclusive(name_config, path_restore)
  if (!missing(name_config)) {
    if (!tryCatch(is.character(name_config), error = \(e) FALSE)) {
      name_config <- deparse1(substitute(name_config))
    }
    targets_data_fetch <- list(
      tar_target_raw(
        paste("config_where_single_game", name_suffix, sep = "_"),
        rlang::expr(
          insert_where_single_game(
            !!rlang::ensym(name_config),
            game_id
          )
        )
      ),
      tar_target_raw(
        paste("data", name_suffix, sep = "_"),
        rlang::expr(
          pickup(
            query_tmpl_data,
            !!rlang::sym(
              paste("config_where_single_game", name_suffix, sep = "_")
            )
          )
        )
      ),
      tar_target_raw(
        paste("data_parsed", name_suffix, sep = "_"),
        rlang::expr(
          wrangle_data(
            !!rlang::sym(paste("data", name_suffix, sep = "_"))
          )
        )
      )
    )
  }
  if (!missing(path_restore)) {
    targets_data_fetch <- list(
      tar_target_raw(
        paste("file_data", name_suffix, sep = "_"),
        rlang::expr(
          fs::path(!!path_restore, paste("data", game_name_abbr, sep = "_"))
        ),
        format = "file"
      ),
      tar_target_raw(
        paste("data_parsed", name_suffix, sep = "_"),
        rlang::expr(
          wrangle_data(
            qs::qread(
              !!rlang::sym(paste("file_data", name_suffix, sep = "_"))
            )
          )
        )
      )
    )
  }
  tarchetypes::tar_map(
    values = games,
    names = game_name_abbr,
    targets_data_fetch,
    tar_target_raw(
      paste("data_valid", name_suffix, sep = "_"),
      rlang::expr(
        validate_raw_parsed(
          !!rlang::sym(paste("data_parsed", name_suffix, sep = "_")),
          games_req_kb
        )
      )
    ),
    tar_target_raw(
      paste("indices", name_suffix, sep = "_"),
      rlang::expr(
        if (!is.na(prep_fun_name)) {
          preproc_data(
            !!rlang::sym(paste("data_valid", name_suffix, sep = "_")),
            prep_fun,
            .input = input,
            .extra = extra
          )
        }
      )
    )
  )
}
