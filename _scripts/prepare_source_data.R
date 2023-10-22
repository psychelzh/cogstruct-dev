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
      )
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
  tar_target(data_parsed, wrangle_data(data_full))
)

list(
  targets_current,
  targets_main
)
