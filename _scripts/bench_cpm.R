library(targets)
tar_source()
tar_option_set(
  packages = c("tidyverse", "bit64"),
  format = "qs",
  controller = setup_crew_controller("bench_cpm")
)
setup_parallel_plan()
config_cpm <- prepare_config_cpm()
cpm_branches <- tarchetypes::tar_map(
  config_cpm,
  names = !starts_with("file"),
  tarchetypes::tar_rep(
    cpm_result,
    cpmr::cpm(
      qs::qread(file_fc)[subjs_to_keep, ],
      scores_rapm[subjs_to_keep, ],
      confounds = match_confounds(
        users_confounds,
        qs::qread(file_fd)[, 1, drop = FALSE]
      )[subjs_to_keep, ],
      thresh_method = thresh_method,
      thresh_level = thresh_level,
      kfolds = 10
    ),
    batches = 4,
    reps = 5,
    iteration = "list",
    retrieval = "worker",
    storage = "worker"
  ),
  tarchetypes::tar_rep2(
    cpm_performance,
    extract_cpm_performance(cpm_result),
    cpm_result,
    retrieval = "worker",
    storage = "worker"
  )
)
list(
  tarchetypes::tar_file_read(
    scores_rapm,
    path_obj_from_proj("indices_rapm", "prepare_source_data"),
    read = qs::qread(!!.x) |>
      column_to_rownames("user_id") |>
      as.matrix()
  ),
  tarchetypes::tar_file_read(
    users_confounds,
    path_obj_from_proj("users_confounds", "prepare_source_data"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    subjs_keep_neural,
    path_obj_from_proj("subjs_keep_neural", "prepare_neural"),
    read = qs::qread(!!.x)
  ),
  tar_target(
    subjs_to_keep,
    intersect(
      as.character(subjs_keep_neural),
      rownames(scores_rapm)
    )
  ),
  tar_prep_files_cpm(which_fc = "fc_run1"),
  cpm_branches,
  tarchetypes::tar_combine(
    cpm_performance,
    cpm_branches$cpm_performance,
    command = bind_rows_meta(
      !!!.x,
      .names = names(select(config_cpm, !starts_with("file"))),
      .prefix = "cpm_performance"
    )
  )
)
