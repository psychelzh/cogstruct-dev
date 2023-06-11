library(targets)
future::plan(future.callr::callr)
tar_source()
tar_option_set(
  package = c(
    "tidyverse", "tarflow.iquizoo", "bit64",
    "preproc.iquizoo", "lavaan"
  ),
  format = "qs",
  imports = "preproc.iquizoo",
  memory = "transient",
  garbage_collection = TRUE,
  controller = crew::crew_controller_local(workers = 10)
)
search_games_mem <- memoise::memoise(
  tarflow.iquizoo::search_games,
  cache = cachem::cache_disk("~/.cache.tarflow")
)
configs <- c("main", "makeup", "restore")
games_config <- configs |>
  rlang::set_names() |>
  lapply(
    \(config) {
      search_games_mem(
        config_where = config::get("where", config = config)
      ) |>
        dplyr::mutate(
          prep_fun = dplyr::if_else(
            game_name == "文字推理",
            rlang::syms("countcorrect_vr"),
            prep_fun
          )
        )
    }
  )
targets_data <- lapply(
  configs,
  \(config) {
    if (config != "restore") {
      prepare_data(
        games_config[[config]],
        name_config = paste("config_where", config, sep = "_"),
        name_suffix = config
      )
    } else {
      prepare_data(
        games_config[[config]],
        path_restore = here::here(
          "../archived/cogstruct-dev-archived/_targets/preproc_behav/objects"
        ),
        name_suffix = config
      )
    }
  }
)

targets_slices <- games_config |>
  dplyr::bind_rows(.id = "config") |>
  dplyr::mutate(
    tar_data_valid = stringr::str_c(
      "data_valid", config, game_name_abbr,
      sep = "_"
    ) |>
      rlang::syms()
  ) |>
  tidyr::chop(c(config, tar_data_valid)) |>
  dplyr::left_join(
    readr::read_csv("config/game_format.csv", show_col_types = FALSE),
    by = "game_name"
  ) |>
  dplyr::mutate(
    slice_data_fun = purrr::map(
      format,
      ~ if (.x %in% c("trials", "items", "duration")) {
        rlang::sym(paste0("slice_data_", .x))
      }
    )
  ) |>
  tarchetypes::tar_map(
    names = game_name_abbr,
    list(
      tar_target(
        data_valid_slices,
        if (!is.null(slice_data_fun)) {
          slice_data_fun(
            bind_rows(tar_data_valid),
            subset = subset,
            parts = parts
          )
        }
      ),
      tar_target(
        indices_slices,
        if (!is.null(data_valid_slices)) {
          tarflow.iquizoo::preproc_data(
            data_valid_slices,
            prep_fun,
            .input = input,
            .extra = extra
          )
        }
      )
    )
  )

list(
  tar_target(game_ids, unique(indices$game_id)),
  tarchetypes::tar_file_read(
    formats,
    "config/game_format.csv",
    read = read_csv(!!.x, show_col_types = FALSE)
  ),
  tar_target(file_config, "config.yml", format = "file"),
  lapply(
    configs,
    \(config)
    tar_target_raw(
      paste("config_where", config, sep = "_"),
      rlang::expr(
        config::get("where", file = file_config, config = !!config)
      )
    )
  ),
  tar_target(games_req_kb, config::get("require_keyboard", file = file_config)),
  tarchetypes::tar_file_read(
    users,
    fs::path("sql", "users.tmpl.sql"),
    read = pickup(!!.x, config_where_main)
  ),
  tarchetypes::tar_file_read(
    users_project_progress,
    fs::path("sql", "progress.tmpl.sql"),
    read = pickup(!!.x, config_where_main)
  ),
  tar_target(
    query_tmpl_data,
    fs::path("sql", "data.tmpl.sql"),
    format = "file"
  ),
  tar_target(
    users_completed,
    users_project_progress |>
      filter(str_detect(project_name, "^认知实验[A-E]$")) |>
      summarise(n = sum(project_progress) / 100, .by = user_id) |>
      filter(n >= 4)
  ),
  targets_data,
  tarchetypes::tar_combine(
    data_parsed,
    purrr::list_flatten(targets_data)[
      paste("data_parsed", configs, sep = "_")
    ]
  ),
  tarchetypes::tar_combine(
    indices,
    purrr::list_flatten(targets_data)[
      paste("indices", configs, sep = "_")
    ]
  ),
  tar_target(
    indices_clean,
    clean_indices(indices, users_completed)
  ),
  targets_slices,
  tarchetypes::tar_combine(
    indices_slices,
    targets_slices$indices_slices
  ),
  tar_target(
    indices_slices_clean,
    clean_indices(indices_slices, users_completed, id_cols = c(user_id, part))
  ),
  tarchetypes::tar_file_read(
    config_indices,
    "config/indices_filtering.csv",
    read = read_csv(!!.x, show_col_types = FALSE) |>
      filter(!is.na(dimension)) |>
      mutate(game_index = str_c(game_name_abbr, index_name, sep = "."))
  ),
  tar_target(
    indices_of_interest,
    config_indices |>
      inner_join(
        indices_clean,
        join_by(game_name, game_name_abbr, index_name)
      ) |>
      mutate(score_adj = if_else(reversed, -score, score)) |>
      mutate(
        is_outlier_iqr = score %in% boxplot.stats(score)$out,
        .by = game_index
      )
  ),
  tar_target(
    indices_slices_of_interest,
    config_indices |>
      inner_join(
        indices_slices_clean,
        join_by(game_name, game_name_abbr, index_name)
      ) |>
      mutate(score_adj = if_else(reversed, -score, score)) |>
      mutate(
        is_outlier_iqr = score %in% boxplot.stats(score)$out,
        .by = c(game_index, part)
      )
  ),
  tar_target(
    indices_wider_clean,
    indices_of_interest |>
      filter(!is_outlier_iqr) |>
      pivot_wider(
        id_cols = user_id,
        names_from = game_index,
        values_from = score_adj
      ) |>
      # currently, these two schools did not complete the makeup tests
      anti_join(
        filter(users, school %in% c("北京大学", "北京联合大学")),
        by = "user_id"
      )
  ),
  tar_target(
    mdl_fitted,
    fit_g(indices_wider_clean, names(indices_wider_clean)[-1])
  ),
  tar_target(
    scores_g,
    predict_g_score(indices_wider_clean, mdl_fitted)
  )
)
