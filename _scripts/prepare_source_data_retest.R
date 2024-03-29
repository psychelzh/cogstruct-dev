# Created by tarflow.iquizoo::use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c("tidyverse", "tarflow.iquizoo", "preproc.iquizoo", "bit64"),
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

targets_preproc <- tarflow.iquizoo::tar_prep_raw(
  contents |>
    dplyr::distinct(game_id) |>
    data.iquizoo::match_preproc(type = "semi", rm_tagged = TRUE),
  name_parsed = "data_valid",
  action_raw_data = "preproc"
)

targets_test_retest <- tar_test_retest(contents)
targets_test_retest_slices <- tar_test_retest(
  contents |>
    dplyr::semi_join(
      dplyr::filter(config_format, !is.na(format)),
      by = "game_id"
    ),
  name_suffix = "slices",
  extra_by = "part"
)

targets_data_names <- tarchetypes::tar_map(
  dplyr::distinct(contents, game_id) |>
    dplyr::mutate(
      game_id = as.character(game_id),
      tar_parsed = rlang::syms(
        sprintf("raw_data_parsed_%s", game_id)
      )
    ),
  names = game_id,
  tar_target(
    data_names,
    tar_parsed |>
      mutate(col_names = map(raw_parsed, names)) |>
      distinct(pick(c("game_id", "col_names")))
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
  targets_data_names,
  tarchetypes::tar_combine(
    data_names,
    targets_data_names$data_names
  ),
  tar_validate_rawdata(contents),
  targets_preproc,
  tarchetypes::tar_combine(indices, targets_preproc$indices),
  targets_test_retest,
  tarchetypes::tar_combine(
    indices_retest,
    targets_test_retest$indices_retest,
    command = bind_rows(!!!.x, .id = ".id") |>
      zutils::separate_wider_dsv(
        ".id", "game_id",
        prefix = "indices_retest"
      ) |>
      mutate(game_id = bit64::as.integer64(game_id))
  ),
  tarchetypes::tar_combine(
    test_retest,
    targets_test_retest$test_retest,
    command = bind_rows(!!!.x, .id = ".id") |>
      zutils::separate_wider_dsv(
        ".id", "game_id",
        prefix = "test_retest"
      ) |>
      mutate(game_id = bit64::as.integer64(game_id))
  ),
  # test retest after partitioning
  tar_partition_rawdata(contents),
  targets_test_retest_slices,
  tarchetypes::tar_combine(
    test_retest_slices,
    targets_test_retest_slices$test_retest,
    command = bind_rows(!!!.x, .id = ".id") |>
      zutils::separate_wider_dsv(
        ".id", "game_id",
        prefix = "test_retest_slices"
      ) |>
      mutate(game_id = bit64::as.integer64(game_id))
  ),
  tar_target(
    test_retest_pool,
    bind_rows(
      test_retest_slices,
      add_column(test_retest, part = 1)
    )
  )
)
