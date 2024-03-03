library(targets)
tar_source()
tar_option_set(
  packages = c("tidyverse", "bit64"),
  format = "qs",
  controller = setup_crew_controller("pred_pheno")
)
setup_parallel_plan()

config_cpm_data <- prepare_config_cpm_data(
  xcpd == "gsr",
  run == "full"
)
cpm_branches <- tarchetypes::tar_map(
  tidyr::expand_grid(
    config_cpm_data,
    hypers_cpm |>
      dplyr::filter(
        thresh_method == "alpha",
        thresh_level == 0.01
      )
  ),
  names = !c(file_fc, fd),
  tar_target(
    cpm_result,
    apply(
      scores, 2,
      perform_cpm,
      fc = qs::qread(file_fc)[subjs_keep_neural, ],
      confounds = match_confounds(users_confounds, fd),
      bias_correct = FALSE,
      thresh_method = thresh_method,
      thresh_level = thresh_level,
      return_edges = "sum"
    ),
    retrieval = "worker",
    storage = "worker"
  ),
  tar_target(
    cpm_performance,
    lapply_tar_batches(
      cpm_result,
      extract_cpm_performance
    ) |>
      list_rbind(names_to = "latent"),
    retrieval = "worker",
    storage = "worker"
  )
)

cpm_branches_perms <- tarchetypes::tar_map(
  tidyr::expand_grid(
    config_cpm_data,
    hypers_cpm
  ) |>
    dplyr::filter(
      task %in% c("am", "wm"),
      thresh_method == "alpha",
      thresh_level == 0.01
    ),
  names = !c(file_fc, fd),
  tarchetypes::tar_rep(
    cpm_result_perm,
    apply(
      scores, 2,
      perform_cpm_perm,
      fc = qs::qread(file_fc)[subjs_keep_neural, ],
      confounds = match_confounds(users_confounds, fd),
      bias_correct = FALSE,
      thresh_method = thresh_method,
      thresh_level = thresh_level
    ),
    batches = 100,
    reps = 10,
    iteration = "list",
    retrieval = "worker",
    storage = "worker"
  ),
  tarchetypes::tar_rep2(
    cpm_performance_perm,
    lapply_tar_batches(
      cpm_result_perm,
      extract_cpm_performance
    ) |>
      list_rbind_tar_batches(names_to = "index"),
    cpm_result_perm,
    retrieval = "worker",
    storage = "worker"
  )
)

list(
  tarchetypes::tar_file_read(
    scores,
    path_obj_from_proj("scores", "cognition_structure"),
    read = qs::qread(!!.x)
  ),
  tar_prepare_cpm_data(config_cpm_data),
  cpm_branches,
  tarchetypes::tar_combine(
    cpm_performance,
    cpm_branches$cpm_performance,
    command = bind_rows_meta(
      !!!.x,
      .names = c(names(config_fc), names(hypers_cpm)),
      .prefix = "cpm_performance"
    ),
    deployment = "main"
  ),
  cpm_branches_perms,
  tarchetypes::tar_combine(
    cpm_performance_perm,
    cpm_branches_perms$cpm_performance_perm,
    command = bind_rows_meta(
      !!!.x,
      .names = c(names(config_fc), names(hypers_cpm)),
      .prefix = "cpm_performance_perm"
    ),
    deployment = "main"
  )
)
