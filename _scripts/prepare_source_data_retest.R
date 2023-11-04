# Created by tarflow.iquizoo::use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c("tarflow.iquizoo", "tidyverse"), # packages that your targets need to run
  imports = "preproc.iquizoo",
  format = "qs", # Optionally set the default storage format. qs is fast.
  error = "null",
  #
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 8 workers which will run as local R processes:
  controller = crew::crew_controller_local(workers = 8)
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

games_keyboard <- readr::read_lines("config/games_keyboard")
contents <- tarflow.iquizoo:::fetch_iquizoo_mem(
  readr::read_file("sql/contents_with_retest.sql")
)
targets_valid_raw <- tarchetypes::tar_map(
  values = contents |>
    dplyr::distinct(game_id) |>
    dplyr::inner_join(data.iquizoo::game_info, by = "game_id") |>
    dplyr::mutate(
      game_id = as.character(game_id),
      require_keyboard = game_name %in% games_keyboard,
      tar_parsed = rlang::syms(
        stringr::str_glue("raw_data_parsed_{game_id}")
      )
    ),
  names = game_id,
  tar_target(
    data_valid,
    validate_data(tar_parsed, require_keyboard)
  )
)
targets_preproc <- tarflow.iquizoo:::tar_action_raw_data(
  contents |>
    dplyr::distinct(game_id),
  name_parsed = "data_valid",
  action_raw_data = "preproc",
  add_combine_pre = FALSE
)

targets_reliabilty <- tarchetypes::tar_map(
  values = contents |>
    dplyr::distinct(game_id) |>
    data.iquizoo::match_preproc(type = "semi") |>
    dplyr::left_join(data.iquizoo::game_info, by = "game_id") |>
    dplyr::mutate(
      game_id_rel = dplyr::coalesce(game_id_parallel, game_id)
    ) |>
    dplyr::summarise(
      tar_indices = rlang::syms(
        stringr::str_glue("indices_{game_id}")
      ) |>
        list(),
      .by = game_id_rel
    ) |>
    dplyr::mutate(game_id_rel = as.character(game_id_rel)),
  names = game_id_rel,
  tar_target(
    reliability,
    calc_reliability(bind_rows(tar_indices))
  )
)

# Replace the target list below with your own:
list(
  tarflow.iquizoo::tar_prep_iquizoo(
    contents = contents,
    what = "raw_data", # change to "scores" or "raw_data" if you want to
    action_raw_data = "parse",
    check_progress = FALSE # set as `FALSE` if projects finalized
  ),
  # more targets goes here
  tar_prep_creativity(),
  targets_valid_raw,
  targets_preproc,
  targets_reliabilty,
  tarchetypes::tar_combine(
    reliability,
    targets_reliabilty$reliability
  )
)