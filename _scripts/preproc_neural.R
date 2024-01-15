library(targets)
tar_option_set(
  packages = c("tidyverse", "bit64"),
  format = "qs",
  controller = crew.cluster::crew_controller_sge(
    name = "prep_neural",
    workers = 40
  )
)
tar_source()

list(
  tarchetypes::tar_map(
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
  ),
  tarchetypes::tar_map(
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
)
