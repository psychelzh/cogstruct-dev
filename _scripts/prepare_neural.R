library(targets)
tar_source()
tar_option_set(
  packages = c("tidyverse", "bit64", "lavaan"),
  format = "qs",
  controller = setup_crew_controller("fc")
)
setup_parallel_plan()

# prepare functional connectivity (FC) data ----
targets_fc <- tarchetypes::tar_map(
  tidyr::expand_grid(
    params_xcpd,
    params_fmri_tasks,
    params_atlas
  ),
  tar_target(
    meta_time_series,
    prepare_meta_time_series(config, session, task, atlas)
  ),
  tar_target(fc, prepare_data_fc(meta_time_series)),
  tar_target(fc_run1, prepare_data_fc(meta_time_series, run == 1))
)

# extract mean frame-wise displacement (FD) ----
targets_fd <- tarchetypes::tar_map(
  params_fmri_tasks,
  tar_target(
    meta_data_motion,
    prepare_meta_data_motion(session, task)
  ),
  tar_target(
    fd_mean,
    prepare_fd_mean(meta_data_motion)
  )
)

list(
  targets_fc,
  targets_fd,
  # criteria for keeping subjects ---
  # 1. no run with mean FD > 0.5
  # 2. no missing runs
  tarchetypes::tar_combine(
    fd_mean,
    targets_fd$fd_mean,
    command = do.call(cbind, list(!!!.x))
  ),
  tar_target(
    subjs_keep_neural,
    fd_mean |>
      apply(1, \(x) all(x <= 0.5) && all(!is.na(x))) |>
      which() |>
      names()
  )
)
