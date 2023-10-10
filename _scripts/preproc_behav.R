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
games_config <- rlang::set_names(c("main", "makeup", "restore")) |>
  lapply(
    \(config) {
      search_games_mem(
        config_where = config::get("where", config = config)
      )
    }
  )
targets_data <- games_config |>
  purrr::imap(
    \(games, config) {
      config_where <- tar_target_raw(
        paste("config_where", config, sep = "_"),
        rlang::expr(
          config::get("where", file = file_config, config = !!config)
        )
      )
      data <- if (config != "restore") {
        prepare_data(
          games,
          name_config = paste("config_where", config, sep = "_"),
          name_suffix = config
        )
      } else {
        prepare_data(
          games,
          path_restore = here::here(
            "../archived/cogstruct-dev-archived/_targets/preproc_behav/objects"
          ),
          name_suffix = config
        )
      }
      c(config_where, data)
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
  dplyr::inner_join(
    readr::read_csv("config/game_format.csv", show_col_types = FALSE),
    by = "game_name"
  ) |>
  dplyr::filter(!is.na(format)) |>
  dplyr::mutate(
    slice_data_fun = rlang::syms(paste0("slice_data_", format)),
    prep_fun = ifelse(
      game_name == "社交达人",
      rlang::syms("fname_slices"),
      prep_fun
    )
  ) |>
  split(~format) |>
  purrr::imap(
    \(values, format) {
      args_slice <- if (format == "items") {
        # partition based on correlation with scores_g
        "scores_g"
      } else {
        c("subset", "parts")
      }
      args_slice <- setNames(rlang::syms(args_slice), args_slice)
      tarchetypes::tar_map(
        values,
        names = game_name_abbr,
        list(
          tar_target_raw(
            "data_valid_slices",
            rlang::expr(
              slice_data_fun(
                bind_rows(tar_data_valid),
                !!!args_slice
              )
            )
          ),
          tar_target(
            indices_slices,
            tarflow.iquizoo::preproc_data(
              data_valid_slices,
              prep_fun,
              .input = input,
              .extra = extra
            )
          )
        )
      )
    }
  )

list(
  tar_target(game_ids, unique(indices$game_id)),
  tar_target(file_config, "config.yml", format = "file"),
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
    purrr::imap(
      targets_data,
      ~ .x[paste("data_parsed", .y, sep = "_")]
    )
  ),
  tarchetypes::tar_combine(
    indices,
    purrr::imap(
      targets_data,
      ~ .x[paste("indices", .y, sep = "_")]
    )
  ),
  tar_target(
    indices_clean,
    clean_indices(indices, users_completed)
  ),
  targets_slices,
  tarchetypes::tar_combine(
    indices_slices,
    targets_slices |> purrr::map("indices_slices")
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
