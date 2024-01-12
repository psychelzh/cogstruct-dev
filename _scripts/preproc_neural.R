library(targets)
tar_option_set(
  packages = c("tidyverse", "bit64"),
  format = "qs",
  controller = crew::crew_controller_local(
    name = "liang",
    workers = 16
  )
)
tar_source()

fc_preparation <- tarchetypes::tar_map(
  tidyr::expand_grid(
    hypers_xcpd_config,
    hypers_fmri_dataset
  ),
  tar_target(
    ts_files,
    prepare_ts_files(config, session, task, atlas)
  ),
  tar_target(
    ts_merged,
    prepare_ts_merged(ts_files)
  ),
  tarchetypes::tar_map(
    tibble::tibble(length = c("full", "equal")) |>
      dplyr::filter(length == "full"),
    tar_target(fc_orig, prepare_fc_data(ts_merged))
  )
)

list(
  fc_preparation
)
