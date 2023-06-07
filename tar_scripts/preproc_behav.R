library(targets)
future::plan(future.callr::callr)
tar_source()
tar_option_set(
  package = c("tidyverse", "preproc.iquizoo", "tarflow.iquizoo", "bit64"),
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
targets_data <- lapply(
  configs,
  \(config) {
    games <- search_games_mem(
      config_where = config::get("where", config = config)
    )
    if (config != "restore") {
      prepare_data(
        games,
        config = paste("config_where", config, sep = "_"),
        name_suffix = config
      )
    } else {
      prepare_data(
        games |> dplyr::filter(game_name_abbr != "RAT"),
        path_restore = here::here(
          "../archived/cogstruct-dev-archived/_targets/preproc_behav/objects"
        ),
        name_suffix = config
      )
    }
  }
)

list(
  tar_target(game_ids, unique(indices$game_id)),
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
  tarchetypes::tar_file_read(
    config_indices,
    "config/indices_filtering.csv",
    read = read_csv(!!.x, show_col_types = FALSE) |>
      filter(!is.na(dimension)) |>
      mutate(game_index = str_c(game_name_abbr, index_name, sep = "."))
  ),
  tar_target(
    indices_clean,
    clean_indices(indices, users_completed)
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
  )
)
