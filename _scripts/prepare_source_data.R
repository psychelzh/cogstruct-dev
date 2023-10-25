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
targets_main <- tarchetypes::tar_map(
  values = contents |>
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
      require_keyboard = game_name %in% games_keyboard
    ),
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
  tar_target(data_parsed, wrangle_data(data_full)),
  tar_target(data_valid, validate_data(data_parsed, require_keyboard))
)
targets_preproc <- tarflow.iquizoo:::tar_action_raw_data(
  contents |>
    dplyr::distinct(game_id) |>
    dplyr::mutate(
      tar_data = rlang::syms(
        stringr::str_glue("data_valid_{game_id}")
      )
    ),
  name_parsed = "tar_data", # workaround by use name from values
  action_raw_data = "preproc",
  add_combine_pre = FALSE
)

list(
  targets_current,
  targets_main,
  targets_preproc,
  tarchetypes::tar_file_read(
    users_project_progress,
    "sql/progress.tmpl.sql",
    read = tarflow.iquizoo::fetch_iquizoo(
      read_file(!!.x),
      params = list(unique(contents_origin$project_id))
    )
  )
)
