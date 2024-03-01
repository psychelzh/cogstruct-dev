library(targets)
tar_source()
tar_option_set(
  packages = c("tidyverse", "bit64", "lavaan"),
  format = "qs",
  controller = setup_crew_controller("fc")
)
setup_parallel_plan()
targets_fd <- tarchetypes::tar_map(
  params_fmri_tasks,
  names = !runs,
  tar_target(
    meta_data_motion,
    prepare_meta_data_motion(session, task)
  ),
  tar_target(
    fd_mean,
    prepare_fd_mean(meta_data_motion)
  )
)
config_meta <- config_fmri |>
  dplyr::select(!runs) |>
  tidyr::unite("name_suffix", everything(), remove = FALSE) |>
  dplyr::mutate(
    meta_time_series = rlang::syms(
      sprintf("meta_time_series_%s", name_suffix)
    )
  )
config_fc_calc <- config_fc |>
  dplyr::left_join(config_meta, by = c("xcpd", "session", "task", "atlas")) |>
  dplyr::mutate(fc = rlang::syms(sprintf("fc_%s_%s", name_suffix, run)))
list(
  # prepare functional connectivity (FC) data ----
  tarchetypes::tar_eval(
    tar_target(
      meta_time_series,
      prepare_meta_time_series(xcpd, session, task, atlas)
    ),
    config_meta
  ),
  tarchetypes::tar_eval(
    tar_target(fc, prepare_data_fc(meta_time_series)),
    dplyr::filter(config_fc_calc, run == "full")
  ),
  tarchetypes::tar_eval(
    tar_target(
      fc,
      prepare_data_fc(meta_time_series, .data[["run"]] %in% parse_digits(run))
    ),
    dplyr::filter(config_fc_calc, grepl("^run\\d+$", run))
  ),
  # extract mean frame-wise displacement (FD) ----
  targets_fd,
  tarchetypes::tar_combine(
    fd_mean,
    targets_fd$fd_mean,
    command = do.call(cbind, list(!!!.x))
  ),
  # criteria for keeping subjects ---
  # 1. no run with mean FD > 0.5
  # 2. no missing runs
  tar_target(
    subjs_keep_neural,
    names(which(apply(fd_mean, 1, \(x) all(x <= 0.5) && all(!is.na(x)))))
  )
)
