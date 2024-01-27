library(targets)
tar_source()
setup_targets_options(
  "fc",
  packages = c("tidyverse", "bit64"),
  format = "qs"
)
setup_targets_parallel()

list(
  tarchetypes::tar_map(
    tidyr::expand_grid(
      params_fmri_tasks,
      params_xcpd
    ),
    tar_target(
      files_ts,
      prepare_files_ts(session, task, config, atlas)
    ),
    tar_target(ts_merged, prepare_ts_merged(files_ts)),
    tar_target(fc_orig_full, prepare_data_fc(ts_merged))
  ),
  tarchetypes::tar_map(
    params_fmri_tasks,
    tar_target(
      files_confounds,
      prepare_files_confounds(session, task)
    ),
    tar_target(
      confounds,
      prepare_data_confounds(files_confounds)
    ),
    tar_target(fd_mean, prepare_fd_mean(confounds)),
    tar_target(
      confounds_cpm,
      compose_confounds_cpm(users_demography, fd_mean)
    )
  ),
  tarchetypes::tar_file_read(
    users_demography,
    path_obj_from_proj("users_demography", "prepare_source_data"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_eval(
    tar_target(
      subjs_keep_neural,
      bind_rows(expr_join_fd) |>
        filter(all(fd <= 0.3), .by = user_id) |>
        pull(user_id) |>
        unique()
    ),
    params_fmri_tasks |>
      dplyr::summarise(
        expr_join_fd = c(
          quote(list),
          rlang::syms(
            sprintf("fd_mean_%s_%s", session, task)
          )
        ) |>
          as.call() |>
          list()
      )
  )
)
