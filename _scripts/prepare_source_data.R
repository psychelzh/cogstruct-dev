library(targets)
future::plan(future.callr::callr)
tar_source()
tar_option_set(
  package = c("tidyverse", "bit64", "lavaan"),
  format = "qs",
  imports = "preproc.iquizoo",
  memory = "transient",
  garbage_collection = TRUE,
  controller = crew::crew_controller_local(workers = 8)
)

path_archive <- Sys.getenv("OneDriveConsumer") |>
  fs::path("Documents/Research/archived/cogstruct-dev-archived")
path_restore <- withr::with_dir(
  path_archive,
  fs::path(
    path_archive,
    tar_config_get("store", project = "preproc_behav")
  )
)
contents <- tarflow.iquizoo:::fetch_iquizoo_mem(
  readr::read_file("sql/contents_camp.sql")
)
games_keyboard <- readr::read_lines("config/games_keyboard")
targets_current <- tarflow.iquizoo::tar_prep_iquizoo(
  contents = contents,
  what = "raw_data",
  action_raw_data = "none",
  check_progress = FALSE
)
config_contents <- contents |>
  dplyr::distinct(game_id) |>
  dplyr::left_join(data.iquizoo::game_info, by = "game_id") |>
  dplyr::mutate(
    game_id = as.character(game_id),
    name_current = rlang::syms(
      stringr::str_glue("raw_data_{game_id}")
    ),
    name_restore = rlang::syms(
      stringr::str_glue("data_{game_name_abbr}")
    ),
    require_keyboard = game_name %in% games_keyboard,
    tar_parsed = rlang::syms(
      stringr::str_glue("data_parsed_{game_id}")
    )
  )
targets_main <- tarchetypes::tar_map(
  values = config_contents,
  names = game_id,
  tar_target(
    data_full,
    bind_rows(
      select(name_current, -project_id),
      purrr::possibly(
        \(...) {
          select(
            targets::tar_read(...),
            !contains("name")
          )
        }
      )(name_restore, store = path_restore)
    ) |>
      distinct()
  ),
  tar_target(data_parsed, wrangle_data(data_full))
)
targets_valid_raw <- list(
  tarchetypes::tar_map(
    values = config_contents |>
      dplyr::filter(game_id != "380174879445893"),
    names = game_id,
    tar_target(
      data_valid,
      validate_data(tar_parsed, require_keyboard)
    )
  ),
  # special case for game "人工语言": correct accuracy
  tarchetypes::tar_map(
    values = config_contents |>
      dplyr::filter(game_id == "380174879445893"),
    names = game_id,
    tar_target(
      data_valid,
      validate_data(tar_parsed, require_keyboard) |>
        correct_cr(cr_correction)
    )
  )
)
targets_preproc <- tarflow.iquizoo:::tar_action_raw_data(
  contents |>
    dplyr::distinct(game_id),
  name_parsed = "data_valid", # workaround by use name from values
  action_raw_data = "preproc",
  add_combine_pre = FALSE
)

list(
  targets_current,
  targets_main,
  targets_valid_raw,
  targets_preproc,
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
  ),
  tarchetypes::tar_file_read(
    users_project_progress,
    "sql/progress.tmpl.sql",
    read = tarflow.iquizoo::fetch_iquizoo(
      read_file(!!.x),
      params = list(unique(contents_origin$project_id))
    )
  ),
  tar_target(
    users_completed,
    users_project_progress |>
      filter(str_detect(project_name, "^认知实验[A-E]$")) |>
      summarise(n = sum(project_progress) / 100, .by = user_id) |>
      filter(n >= 4)
  ),
  tar_target(
    indices_clean,
    clean_indices(indices, users_completed)
  ),
  tar_target(
    indices_of_interest,
    indices_clean |>
      inner_join(
        data.iquizoo::game_indices,
        join_by(game_id, index_name == index_main)
      ) |>
      mutate(score_adj = if_else(index_reverse, -score, score)) |>
      mutate(
        is_outlier_iqr = score %in% boxplot.stats(score)$out,
        .by = c(game_id, index_name)
      )
  ),
  tar_target(
    indices_wider_clean,
    indices_of_interest |>
      filter(!is_outlier_iqr) |>
      mutate(game_index = str_c(game_name_abbr, index_name, sep = ".")) |>
      pivot_wider(
        id_cols = user_id,
        names_from = game_index,
        values_from = score_adj
      )
  )
)
