library(targets)
tar_option_set(
  packages = c("tidyverse", "bit64"),
  format = "qs",
  controller = if (Sys.info()["nodename"] == "shadow") {
    crew.cluster::crew_controller_sge(
      name = "fc",
      workers = 40,
      seconds_idle = 30
    )
  } else {
    crew::crew_controller_local(
      name = "fc-local",
      workers = 16,
      seconds_idle = 10
    )
  }
)
tar_source()

list(
  tarchetypes::tar_map(
    hypers_fc,
    tar_target(
      files_ts,
      prepare_files_ts(config, session, task, atlas)
    ),
    tar_target(ts_merged, prepare_ts_merged(files_ts)),
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
    ),
    tar_target(
      confounds_cpm,
      prepare_data_confounds_cpm(confounds, users_demography)
    )
  ),
  tar_target(
    file_users,
    path_obj_from_proj("users", "prepare_source_data"),
    format = "file"
  ),
  tar_target(
    file_indices,
    path_obj_from_proj("indices", "prepare_source_data"),
    format = "file"
  ),
  tar_target(
    users_demography,
    prepare_users_demography(file_users, file_indices)
  )
)
