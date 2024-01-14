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

# these external data paths are specially for current pipeline
root_bids_xcpd <- r"(F:\CAMP)"
root_bids_fmriprep <- r"(Z:\seastor\CAMP\derivatives\fmriprep)"

fc_preparation <- tarchetypes::tar_map(
  tidyr::expand_grid(
    hypers_xcpd_config,
    hypers_fmri_dataset,
    hypers_atlas
  ),
  tar_target(
    files_ts,
    prepare_files_ts(config, session, task, atlas)
  ),
  tar_target(
    ts_merged,
    prepare_ts_merged(files_ts)
  ),
  tar_target(fc_orig_full, prepare_data_fc(ts_merged))
)

confounds_preparation <- tarchetypes::tar_map(
  hypers_fmri_dataset,
  tar_target(
    files_confounds,
    prepare_files_confounds(session, task)
  ),
  tar_target(
    confounds,
    prepare_data_confounds(files_confounds)
  )
)

list(
  fc_preparation,
  confounds_preparation
)
