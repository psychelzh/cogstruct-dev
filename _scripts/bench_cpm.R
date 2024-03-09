library(targets)
tar_source()
tar_option_set(
  packages = c("tidyverse", "bit64", "lavaan"),
  format = "qs",
  controller = setup_crew_controller("bench_cpm")
)
setup_parallel_plan()
bench_indices <- tibble::tibble(
  index = c("rapm", "g"),
  scores = rlang::syms(sprintf("scores_%s", index))
)
config_cpm_data <- prepare_config_cpm_data()
config_cpm <- tidyr::expand_grid(
  config_cpm_data,
  hypers_cpm,
  bench_indices
)
names_exclude <- c(names_exclude, "scores")
cpm_branches <- tarchetypes::tar_map(
  config_cpm,
  names = !all_of(names_exclude),
  tar_target(
    cpm_result,
    perform_cpm(
      qs::qread(file_fc)[subjs_keep_neural, ],
      scores,
      match_confounds(users_confounds, fd),
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
    extract_cpm_performance(cpm_result),
    retrieval = "worker",
    storage = "worker"
  )
)

cpm_branches_perms <- tarchetypes::tar_map(
  config_cpm |>
    dplyr::filter(
      xcpd == "gsr",
      index == "g",
      thresh_method == "alpha",
      thresh_level == 0.01
    ),
  names = !all_of(names_exclude),
  tarchetypes::tar_rep(
    cpm_result_perm,
    perform_cpm_perm(
      qs::qread(file_fc)[subjs_keep_neural, ],
      scores,
      match_confounds(users_confounds, fd),
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
    extract_cpm_performance(cpm_result_perm),
    cpm_result_perm,
    retrieval = "worker",
    storage = "worker"
  )
)

list(
  tarchetypes::tar_file_read(
    scores_rapm,
    path_obj_from_proj("indices_rapm", "prepare_source_data"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    scores_g,
    path_obj_from_proj("scores_g_full", "g_factor"),
    read = qs::qread(!!.x)
  ),
  tar_prepare_cpm_data(config_cpm_data),
  cpm_branches,
  tarchetypes::tar_combine(
    cpm_performance,
    cpm_branches$cpm_performance,
    command = bind_rows_meta(
      !!!.x,
      .names = setdiff(names(config_cpm), names_exclude),
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
      .names = setdiff(names(config_cpm), names_exclude),
      .prefix = "cpm_performance_perm"
    ),
    deployment = "main"
  )
)
