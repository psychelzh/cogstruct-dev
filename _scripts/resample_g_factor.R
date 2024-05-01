library(targets)
tar_source()
tar_option_set(
  packages = c("tidyverse", "bit64", "lavaan"),
  format = "qs",
  controller = setup_crew_controller("gf")
)
setup_parallel_plan()

config_neural <- prepare_config_neural(
  xcpd == "gsr",
  task == "wm",
  run == "full",
  atlas == "4S256Parcels"
)
hypers_cpm <- hypers_cpm |>
  dplyr::filter(
    thresh_method == "alpha",
    thresh_level == 0.01
  )

config_vars <- prepare_config_vars(num_vars_total)
targets_num_vars <- tarchetypes::tar_map(
  config_vars,
  tar_calibrate_g(
    resample_vars(names(indices_cogstruct), num_vars, use_pairs),
    indices_cogstruct,
    use_pairs,
    data_crit = list(
      cor_rapm = indices_rapm,
      # use g based on all variables (`[[` is used because of list iteration)
      cor_g = scores_g_full[[1]][[1]][[1]]
    ),
    config_neural = config_neural,
    hypers_cpm = hypers_cpm,
    batches = 10,
    reps = 10
  )
)
targets_full <- tar_calibrate_g(
  list(names(indices_cogstruct)),
  indices_cogstruct,
  use_pairs = FALSE,
  name_suffix = "full",
  data_crit = list(cor_rapm = indices_rapm),
  config_neural = config_neural,
  hypers_cpm = hypers_cpm
)

config_vars_no_rsn <- prepare_config_vars(
  num_vars_total - length(game_id_reasoning),
  use_pairs = FALSE
)
targets_num_vars_no_rsn <- tarchetypes::tar_map(
  config_vars_no_rsn,
  tar_calibrate_g(
    resample_vars(
      setdiff(names(indices_cogstruct), match_game_index(game_id_reasoning)),
      num_vars
    ),
    indices_cogstruct,
    use_pairs,
    name_suffix = "no_rsn",
    data_crit = list(cor_rapm = indices_rapm),
    batches = 10,
    reps = 10
  )
)

vars_domain <- dplyr::pull(game_index_dims, manifest, label_chc_merged)
config_domains <- prepare_config_domain(vars_domain)
targets_domain <- tarchetypes::tar_map(
  config_domains,
  tar_calibrate_g(
    resample_vars_domain(vars_domain, num_domain, num_vars, use_pairs),
    indices_cogstruct,
    use_pairs,
    name_suffix = "domain",
    data_crit = list(
      cor_rapm = indices_rapm,
      # use g based on all variables (`[[` is used because of list iteration)
      cor_g = scores_g_full[[1]][[1]][[1]]
    ),
    config_neural = config_neural,
    hypers_cpm = hypers_cpm,
    batches = 10,
    reps = 10
  )
)

config_vars_load <- prepare_config_vars(
  num_vars_total %/% 2,
  from = 5, step = 5
)
targets_load <- tarchetypes::tar_map(
  data.frame(part = c("high", "low")),
  tar_target(
    vars_pool,
    extract_vars_by_load(
      loadings(fit_g_full[[1]][[1]][[1]]),
      part = part
    )
  ),
  tarchetypes::tar_map(
    config_vars_load,
    tar_calibrate_g(
      resample_vars(vars_pool, num_vars, use_pairs),
      indices_cogstruct,
      use_pairs,
      name_suffix = "load",
      data_crit = list(
        cor_rapm = indices_rapm,
        cor_g = scores_g_full[[1]][[1]][[1]]
      ),
      config_neural = config_neural,
      hypers_cpm = hypers_cpm,
      batches = 10,
      reps = 10
    )
  )
)

config_select_tasks <- tidyr::expand_grid(
  scheme = c("by_load", "by_step"),
  num_vars = seq(3, num_vars_total - 1)
) |>
  dplyr::mutate(vars = rlang::syms(sprintf("vars_sort_%s", scheme)))
targets_select_tasks <- tarchetypes::tar_map(
  config_select_tasks,
  names = !vars,
  tar_calibrate_g(
    list(head(vars, num_vars)),
    indices_cogstruct,
    use_pairs = FALSE,
    name_suffix = "select_tasks",
    data_crit = list(
      cor_g = scores_g_full[[1]][[1]][[1]]
    ),
    config_neural = config_neural,
    hypers_cpm = hypers_cpm
  )
)

targets_cpm_tasks <- tarchetypes::tar_map(
  tidyr::expand_grid(
    config_neural,
    hypers_cpm
  ),
  names = !all_of(names_exclude),
  tar_target(
    cpm_result_tasks,
    apply(
      indices_cogstruct, 2,
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
    cpm_performance_tasks,
    lapply(cpm_result_tasks, extract_cpm_performance) |>
      list_rbind(names_to = "game_index"),
    retrieval = "worker",
    storage = "worker"
  )
)

list(
  tarchetypes::tar_file_read(
    indices_cogstruct,
    path_obj_from_proj("indices_cogstruct", "prepare_source_data"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    indices_rapm,
    path_obj_from_proj("indices_rapm", "prepare_source_data"),
    read = qs::qread(!!.x)
  ),
  tar_prepare_neural_data(config_neural),
  targets_full,
  tar_combine_branches(
    "cpm_performance_full",
    branches = targets_full,
    meta_names = c(names(config_fc), names(hypers_cpm))
  ),
  targets_num_vars,
  lapply(
    c("rel_pairs_g", "comp_rel_g", "cor_rapm", "cor_g"),
    tar_combine_branches,
    branches = targets_num_vars,
    meta_names = names(config_vars)
  ),
  lapply(
    c("cpm_performance", "dice_pairs"),
    tar_combine_branches,
    branches = targets_num_vars,
    meta_names = c(names(config_fc), names(hypers_cpm), names(config_vars))
  ),
  tar_calibrate_g(
    list(
      setdiff(names(indices_cogstruct), match_game_index(game_id_reasoning))
    ),
    indices_cogstruct,
    use_pairs = FALSE,
    name_suffix = "no_rsn_full",
    data_crit = list(cor_rapm = indices_rapm)
  ),
  targets_num_vars_no_rsn,
  tar_combine_branches(
    "cor_rapm_no_rsn",
    branches = targets_num_vars_no_rsn,
    meta_names = names(config_vars_no_rsn)
  ),
  targets_domain,
  lapply(
    c(
      "rel_pairs_g_domain",
      "comp_rel_g_domain",
      "cor_rapm_domain",
      "cor_g_domain"
    ),
    tar_combine_branches,
    branches = targets_domain,
    meta_names = names(config_domains)
  ),
  lapply(
    c("cpm_performance_domain", "dice_pairs_domain"),
    tar_combine_branches,
    branches = targets_domain,
    meta_names = c(names(config_fc), names(hypers_cpm), names(config_domains))
  ),
  targets_load,
  lapply(
    c("rel_pairs_g_load", "comp_rel_g_load", "cor_rapm_load", "cor_g_load"),
    tar_combine_branches,
    branches = targets_load,
    meta_names = c(names(config_vars_load), "part")
  ),
  lapply(
    c("cpm_performance_load", "dice_pairs_load"),
    tar_combine_branches,
    branches = targets_load,
    meta_names = c(
      names(config_fc),
      names(hypers_cpm),
      names(config_vars_load),
      "part"
    )
  ),
  targets_select_tasks,
  lapply(
    c("comp_rel_g_select_tasks", "cor_g_select_tasks"),
    tar_combine_branches,
    branches = targets_select_tasks,
    meta_names = setdiff(names(config_select_tasks), "vars"),
    names_greedy = "scheme"
  ),
  tar_combine_branches(
    "cpm_performance_select_tasks",
    branches = targets_select_tasks,
    meta_names = setdiff(
      c(names(config_fc), names(hypers_cpm), names(config_select_tasks)),
      "vars"
    ),
    names_greedy = "scheme"
  ),
  tar_target(
    step_forward,
    cbind(indices_cogstruct, scores_g_full[[1]][[1]][[1]]) |>
      lm(f1 ~ ., data = _) |>
      olsrr::ols_step_forward_r2()
  ),
  tar_target(
    vars_sort_by_step,
    step_forward$metrics$variable
  ),
  tar_target(
    loadings_all,
    loadings(fit_g_full[[1]][[1]][[1]])
  ),
  tar_target(
    vars_sort_by_load,
    rownames(loadings_all)[order(loadings_all, decreasing = TRUE)]
  ),
  targets_cpm_tasks,
  tarchetypes::tar_combine(
    cpm_performance_tasks,
    targets_cpm_tasks$cpm_performance_tasks,
    command = bind_rows_meta(
      !!!.x,
      .names = c(names(config_fc), names(hypers_cpm)),
      .prefix = "cpm_performance_tasks"
    ),
    deployment = "main"
  )
)
