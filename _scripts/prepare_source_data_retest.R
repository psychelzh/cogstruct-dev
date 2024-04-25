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
    data.iquizoo::merge_preproc(filter_only = TRUE, rm_tagged = TRUE),
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

targets_hddm <- tarchetypes::tar_map(
  fs::dir_ls("data/hddm-retest") |>
    tibble::as_tibble_col("file") |>
    dplyr::mutate(
      game_id = stringr::str_extract(fs::path_file(file), "(?<=game-)[0-9]+"),
    ) |>
    tidyr::chop(file) |>
    dplyr::mutate(
      indices_retest = rlang::syms(sprintf("indices_retest_%s", game_id))
    ),
  names = game_id,
  tarchetypes::tar_file_read(
    indices_retest_hddm,
    file,
    read = list_rbind(lapply(!!.x, extract_hddm_coefs, context = "retest"))
  ),
  tar_target(
    test_retest_hddm,
    indices_retest_hddm |>
      group_by(index_name) |>
      group_modify(calc_test_retest) |>
      ungroup()
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
  ),
  # hddm
  targets_hddm,
  tarchetypes::tar_combine(
    test_retest_hddm,
    targets_hddm$test_retest_hddm,
    command = bind_rows(!!!.x, .id = ".id") |>
      zutils::separate_wider_dsv(
        ".id", "game_id",
        prefix = "test_retest_hddm"
      ) |>
      mutate(game_id = bit64::as.integer64(game_id))
  )
)
