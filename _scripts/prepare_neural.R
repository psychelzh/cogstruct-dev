library(targets)
tar_source()
tar_option_set(
  packages = c("tidyverse", "bit64", "lavaan"),
  format = "qs",
  controller = setup_crew_controller("fc")
)
setup_parallel_plan()

config_fc_calc <- config_fc |>
  tidyr::unite("name_suffix", everything(), remove = FALSE) |>
  dplyr::mutate(
    session = lapply(session, \(x) if (x == "0") character() else x),
    run = lapply(run, parse_digits),
    meta_time_series = rlang::syms(
      paste0("meta_time_series_", name_suffix)
    ),
    fc = rlang::syms(paste0("fc_", name_suffix))
  )

targets_fd <- tarchetypes::tar_map(
  dplyr::distinct(params_fmri_tasks, session, task),
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

list(
  # prepare functional connectivity (FC) data ----
  tarchetypes::tar_eval(
    list(
      tar_target(
        meta_time_series,
        prepare_meta_time_series(xcpd, session, task, atlas, run)
      ),
      tar_target(
        fc,
        prepare_data_fc(meta_time_series)
      )
    ),
    dplyr::filter(config_fc_calc, task != "latent")
  ),
  tarchetypes::tar_eval(
    tar_target(
      fc,
      do.call(abind::abind, c(call_list_fc, along = 3)) |>
        apply(2, \(x) princomp(x)$scores[, 1])
    ),
    config_fc_calc |>
      dplyr::filter(task == "latent") |>
      dplyr::left_join(
        config_fc_calc |>
          dplyr::filter(task != "latent") |>
          dplyr::summarise(
            call_list_fc = list(
              as.call(c(quote(list), fc))
            ),
            .by = c(xcpd, run)
          ),
        by = c("xcpd", "run")
      )
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
