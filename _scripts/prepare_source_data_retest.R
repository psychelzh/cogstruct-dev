# Created by tarflow.iquizoo::use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c("tidyverse", "tarflow.iquizoo", "preproc.iquizoo"),
  imports = "preproc.iquizoo",
  format = "qs", # Optionally set the default storage format. qs is fast.
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

contents <- tarflow.iquizoo::fetch_iquizoo_mem()(
  readr::read_file("sql/contents_with_retest.sql")
)

targets_preproc <- tarflow.iquizoo:::tar_action_raw_data(
  contents |>
    dplyr::distinct(game_id) |>
    data.iquizoo::match_preproc(type = "semi", rm_tagged = TRUE),
  name_parsed = "data_valid",
  action_raw_data = "preproc"
)

targets_reliabilty <- tarchetypes::tar_map(
  values = contents |>
    dplyr::distinct(game_id) |>
    data.iquizoo::match_preproc(type = "semi", rm_tagged = TRUE) |>
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

targets_indices_partitioned <- tar_partition_rawdata(contents)

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
  tar_validate_rawdata(contents),
  targets_preproc,
  tarchetypes::tar_combine(indices, targets_preproc$indices),
  targets_reliabilty,
  tar_combine_with_meta(
    reliability,
    targets_reliabilty$reliability,
    cols_targets = "game_id",
    fun_post = \(.data) .data |> mutate(game_id = bit64::as.integer64(game_id))
  ),
  targets_indices_partitioned
)
