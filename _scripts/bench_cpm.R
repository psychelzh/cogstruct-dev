library(targets)
tar_source()
tar_option_set(
  packages = c("tidyverse", "bit64", "lavaan"),
  format = "qs",
  controller = setup_crew_controller("bench_cpm")
)
setup_parallel_plan()
config_cpm <- prepare_config_cpm()
cpm_branches <- tarchetypes::tar_map(
  config_cpm,
  names = !starts_with("file"),
  tar_target(
    cpm_result,
    apply(
      scores, 2,
      perform_cpm,
      fc = qs::qread(file_fc)[subjs_keep_neural, ],
      confounds = match_confounds(
        users_confounds,
        qs::qread(file_fd)[, 1, drop = FALSE]
      ),
      bias_correct = FALSE,
      thresh_method = thresh_method,
      thresh_level = thresh_level
    ),
    retrieval = "worker",
    storage = "worker"
  ),
  tar_target(
    cpm_performance,
    lapply(cpm_result, extract_cpm_performance) |>
      list_rbind(names_to = "index"),
    cpm_result,
    retrieval = "worker",
    storage = "worker"
  )
)

cpm_branches_perms <- tarchetypes::tar_map(
  dplyr::filter(
    config_cpm,
    thresh_method == "alpha",
    thresh_level == 0.01
  ),
  names = !starts_with("file"),
  tarchetypes::tar_rep(
    cpm_result_perm,
    apply(
      scores, 2,
      perform_cpm_perm,
      fc = qs::qread(file_fc)[subjs_keep_neural, ],
      confounds = match_confounds(
        users_confounds,
        qs::qread(file_fd)[, 1, drop = FALSE]
      ),
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
  tar_target(
    file_rapm,
    path_obj_from_proj("indices_rapm", "prepare_source_data"),
    format = "file_fast"
  ),
  tar_target(
    file_indices,
    path_obj_from_proj("indices_cogstruct", "prepare_source_data"),
    format = "file_fast"
  ),
  tar_target(
    fit_g,
    fit_efa_g(
      qs::qread(file_indices),
      vars = names(qs::qread(file_indices)),
      missing = "ml"
    )
  ),
  tar_target(
    comp_rel_g,
    tibble(comp_rel = unclass(semTools::compRelSEM(fit_g$nf1)))
  ),
  tar_target(
    scores_g,
    extract_g_scores(fit_g, data = qs::qread(file_indices))
  ),
  tar_target(
    scores,
    merge(qs::qread(file_rapm), scores_g, by = "row.names") |>
      column_to_rownames("Row.names")
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
  ),
  cpm_branches_perms,
  tarchetypes::tar_combine(
    cpm_performance_perm,
    cpm_branches_perms$cpm_performance_perm,
    command = bind_rows_meta(
      !!!.x,
      .names = names(select(config_cpm, !starts_with("file"))),
      .prefix = "cpm_performance"
    )
  )
)
