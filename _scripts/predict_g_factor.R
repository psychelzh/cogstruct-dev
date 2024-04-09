library(targets)
tar_source()
tar_option_set(
  packages = c("tidyverse", "bit64", "lavaan"),
  format = "qs",
  controller = setup_crew_controller("bench_cpm")
)
setup_parallel_plan()
config_indices <- tibble::tibble(
  index = c("rapm", "g"),
  scores = rlang::syms(sprintf("scores_%s", index))
)
config_neural <- prepare_config_neural()
config_cpm <- tidyr::expand_grid(
  config_neural,
  hypers_cpm,
  config_indices
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

targets_efficiency <- tarchetypes::tar_map(
  tidyr::expand_grid(
    config_neural,
    params_efficiency
  ),
  names = !any_of(names_exclude),
  tar_target(
    efficiency,
    prepare_efficiency(
      qs::qread(file_fc),
      weighted,
      thresh_level
    ),
    retrieval = "worker",
    storage = "worker"
  ),
  tarchetypes::tar_map(
    config_indices,
    names = !scores,
    tar_target(
      pred_efficiency,
      {
        subjs_keep <- intersect(
          subjs_keep_neural,
          rownames(scores)
        )
        cor.test(efficiency[subjs_keep], scores[subjs_keep, ]) |>
          broom::tidy()
      }
    )
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
  tar_prepare_neural_data(config_neural),
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
  ),
  tarchetypes::tar_file_read(
    stats_brain_volume,
    path_obj_from_proj("stats_brain_volume", "prepare_neural"),
    read = qs::qread(!!.x)
  ),
  tar_target(
    pred_brain_vol,
    {
      subjs_keep <- intersect(
        subjs_keep_neural,
        rownames(scores_g)
      )
      apply(
        stats_brain_volume[subjs_keep, ],
        2,
        \(x) cor.test(x, scores_g[subjs_keep, ]) |> broom::tidy()
      ) |>
        list_rbind(names_to = "index_brain_vol")
    }
  ),
  targets_efficiency,
  tarchetypes::tar_combine(
    pred_efficiency,
    zutils::select_list(targets_efficiency, starts_with("pred_efficiency")),
    command = bind_rows_meta(
      !!!.x,
      .names = setdiff(
        c(names(config_neural), names(params_efficiency)),
        names_exclude
      ),
      .prefix = "pred_efficiency"
    ),
    deployment = "main"
  )
)
