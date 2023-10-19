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

combine_targets <- function(name, targets, cols_targets, fun = NULL) {
  ischar_name <- tryCatch(
    is.character(name) && length(name) == 1L,
    error = function(e) FALSE
  )
  if (!ischar_name) {
    name <- deparse1(substitute(name))
  }
  if (is.null(fun)) {
    fun <- \(x) x
  }
  tarchetypes::tar_combine_raw(
    name,
    targets[[name]],
    command = bquote(
      list(!!!.x) |>
        lapply(.(rlang::as_function(fun))) |>
        bind_rows(.id = "id") |>
        # note there is delimiter after name should be removed too
        mutate(id = str_remove(id, str_c(.(name), "."))) |>
        separate(id, .(cols_targets), convert = TRUE)
    )
  )
}

prepare_data <- function(games, name_config, path_restore,
                         name_suffix = "restore") {
  rlang::check_exclusive(name_config, path_restore)
  add_suffix <- function(name) {
    paste(name, name_suffix, sep = "_")
  }
  if (!missing(name_config)) {
    if (!tryCatch(is.character(name_config), error = \(e) FALSE)) {
      name_config <- deparse1(substitute(name_config))
    }
    targets_data_fetch <- list(
      tar_target_raw(
        add_suffix("config_where_single_game"),
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
            !!rlang::sym(add_suffix("config_where_single_game"))
          )
        )
      ),
      tar_target_raw(
        add_suffix("data_parsed"),
        rlang::expr(
          wrangle_data(
            !!rlang::sym(add_suffix("data"))
          )
        )
      )
    )
  }
  if (!missing(path_restore)) {
    targets_data_fetch <- list(
      tar_target_raw(
        add_suffix("file_data"),
        rlang::expr(
          fs::path(
            !!path_restore,
            paste("data", game_name_abbr, sep = "_")
          )
        ),
        format = "file"
      ),
      tar_target_raw(
        add_suffix("data_parsed"),
        rlang::expr(
          wrangle_data(
            qs::qread(!!rlang::sym(add_suffix("file_data")))
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
      add_suffix("data_valid"),
      rlang::expr(
        validate_raw_parsed(
          !!rlang::sym(add_suffix("data_parsed")),
          games_req_kb
        )
      )
    ),
    tar_target_raw(
      add_suffix("indices"),
      rlang::expr(
        if (!is.na(prep_fun_name)) {
          preproc_data(
            !!rlang::sym(add_suffix("data_valid")),
            prep_fun,
            .input = input,
            .extra = extra
          )
        }
      )
    )
  )
}
