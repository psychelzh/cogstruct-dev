library(targets)
tar_source()
tar_option_set(
  packages = c("tidyverse", "bit64", "lavaan"),
  format = "qs",
  controller = setup_crew_controller("gf")
)
setup_parallel_plan()

config_vars <- prepare_config_vars(num_vars_total)
config_neural <- prepare_config_neural(
  xcpd == "gsr",
  task == "wm",
  run == "full"
)
branches_g <- tarchetypes::tar_map(
  config_vars,
  tarchetypes::tar_rep(
    vars_sample,
    resample_vars(names(indices_cogstruct), num_vars, use_pairs),
    batches = 10,
    reps = 10,
    iteration = "list",
    deployment = "main"
  ),
  tarchetypes::tar_rep2(
    fit_g,
    lapply_tar_batches(
      vars_sample,
      fit_efa_g,
      data = indices_cogstruct,
      missing = "ml"
    ),
    vars_sample,
    iteration = "list"
  ),
  tarchetypes::tar_rep2(
    comp_rel_g,
    lapply_tar_batches(
      fit_g,
      \(x) tibble(comp_rel = unclass(semTools::compRelSEM(x$nf1)))
    ) |>
      list_rbind_tar_batches(names_to = "id_pairs"),
    fit_g,
    iteration = "list"
  ),
  tarchetypes::tar_rep2(
    scores_g,
    lapply_tar_batches(
      fit_g,
      extract_g_scores,
      data = indices_cogstruct
    ),
    fit_g,
    iteration = "list"
  ),
  tarchetypes::tar_rep2(
    rel_pairs_g,
    tibble(
      r = if (use_pairs) {
        cor(
          scores_g[[1]],
          scores_g[[2]],
          use = "pairwise"
        )[, 1]
      }
    ),
    scores_g,
    iteration = "list"
  ),
  tarchetypes::tar_rep2(
    cor_rapm,
    lapply_tar_batches(
      scores_g,
      \(x) {
        indices_rapm |>
          merge(x, by = "row.names") |>
          summarise(r = cor(score, f1, use = "pairwise"))
      }
    ) |>
      list_rbind_tar_batches(names_to = "id_pairs"),
    scores_g,
    iteration = "list"
  ),
  tarchetypes::tar_map(
    tidyr::expand_grid(
      config_neural,
      hypers_cpm |>
        dplyr::filter(
          thresh_method == "alpha",
          thresh_level == 0.01
        )
    ),
    names = !all_of(names_exclude),
    tarchetypes::tar_rep2(
      cpm_result,
      lapply_tar_batches(
        scores_g,
        perform_cpm,
        fc = qs::qread(file_fc),
        confounds = match_confounds(users_confounds, fd),
        subjs_keep_neural = subjs_keep_neural,
        bias_correct = FALSE,
        thresh_method = thresh_method,
        thresh_level = thresh_level,
        return_edges = "sum"
      ),
      scores_g,
      iteration = "list",
      retrieval = "worker",
      storage = "worker"
    ),
    tarchetypes::tar_rep2(
      dice_pairs,
      if (use_pairs) {
        calc_dice_pairs(cpm_result, 0.5)
      } else {
        tibble()
      },
      cpm_result,
      retrieval = "worker",
      storage = "worker"
    ),
    tarchetypes::tar_rep2(
      cpm_performance,
      lapply_tar_batches(
        cpm_result,
        extract_cpm_performance
      ) |>
        list_rbind_tar_batches(names_to = "id_pairs"),
      cpm_result,
      retrieval = "worker",
      storage = "worker"
    )
  )
)

num_pairs_chc <- 20
branches_g_chc <- tarchetypes::tar_map(
  list(id_rsmp = seq_len(num_pairs_chc)),
  tar_target(pairs_chc, resample_pairs_chc()),
  tar_target(vars_chc, extract_vars_chc(pairs_chc)),
  tar_target(num_vars_chc, allocate_num_vars_chc(vars_chc)),
  tar_target(
    vars_rsmp_chc,
    replicate(100, lapply(vars_chc, sample, num_vars_chc), simplify = FALSE),
    pattern = map(num_vars_chc),
    iteration = "list"
  ),
  tar_target(
    fit_g_chc,
    lapply(
      vars_rsmp_chc,
      \(x) lapply(x, fit_efa_g, data = indices_cogstruct, missing = "ml")
    ),
    pattern = map(vars_rsmp_chc),
    iteration = "list"
  ),
  tar_target(
    scores_g_chc,
    lapply(
      fit_g_chc,
      \(x) lapply(x, extract_g_scores, data = indices_cogstruct)
    ),
    pattern = map(fit_g_chc),
    iteration = "list"
  ),
  tar_target(
    rel_pairs_g_chc,
    list_rbind(
      lapply(
        scores_g_chc,
        \(x) tibble(r = as.vector(cor(x[[1]], x[[2]], use = "pairwise")))
      ),
      names_to = "id_rep"
    ),
    pattern = map(scores_g_chc),
    iteration = "list"
  ),
  tarchetypes::tar_map(
    tidyr::expand_grid(
      config_neural,
      hypers_cpm |>
        dplyr::filter(
          thresh_method == "alpha",
          thresh_level == 0.01
        )
    ),
    names = !all_of(names_exclude),
    tar_target(
      cpm_result_chc,
      lapply(
        scores_g_chc,
        \(x) {
          lapply(
            x,
            perform_cpm,
            fc = qs::qread(file_fc),
            confounds = match_confounds(users_confounds, fd),
            subjs_keep_neural = subjs_keep_neural,
            bias_correct = FALSE,
            thresh_method = thresh_method,
            thresh_level = thresh_level,
            return_edges = "sum"
          )
        }
      ),
      pattern = map(scores_g_chc),
      iteration = "list",
      retrieval = "worker",
      storage = "worker"
    ),
    tar_target(
      dice_pairs_chc,
      list_rbind(
        lapply(cpm_result_chc, calc_dice_pairs, 0.5),
        names_to = "id_rep"
      ),
      pattern = map(cpm_result_chc),
      iteration = "list",
      retrieval = "worker",
      storage = "worker"
    ),
    tar_target(
      cpm_performance_chc,
      lapply(
        cpm_result_chc,
        \(result) {
          list_rbind(
            lapply(result, extract_cpm_performance),
            names_to = "id_pairs"
          )
        }
      ) |>
        list_rbind(names_to = "id_rep"),
      pattern = map(cpm_result_chc),
      retrieval = "worker",
      storage = "worker"
    )
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
  branches_g,
  tarchetypes::tar_combine(
    rel_pairs_g,
    branches_g$rel_pairs_g,
    command = list(!!!.x) |>
      lapply(bind_rows) |>
      bind_rows_meta(
        .names = names(config_vars),
        .prefix = "rel_pairs_g"
      ),
    deployment = "main"
  ),
  tarchetypes::tar_combine(
    comp_rel_g,
    branches_g$comp_rel_g,
    command = list(!!!.x) |>
      lapply(bind_rows) |>
      bind_rows_meta(
        .names = names(config_vars),
        .prefix = "comp_rel_g"
      ),
    deployment = "main"
  ),
  tarchetypes::tar_combine(
    cor_rapm,
    branches_g$cor_rapm,
    command = list(!!!.x) |>
      lapply(bind_rows) |>
      bind_rows_meta(
        .names = names(config_vars),
        .prefix = "cor_rapm"
      ),
    deployment = "main"
  ),
  tarchetypes::tar_combine(
    cpm_performance,
    zutils::select_list(branches_g, starts_with("cpm_performance")),
    command = bind_rows_meta(
      !!!.x,
      .names = c(names(config_fc), names(hypers_cpm), names(config_vars)),
      .prefix = "cpm_performance"
    ),
    deployment = "main"
  ),
  tarchetypes::tar_combine(
    dice_pairs,
    zutils::select_list(branches_g, starts_with("dice_pairs")),
    command = bind_rows_meta(
      !!!.x,
      .names = c(names(config_fc), names(hypers_cpm), names(config_vars)),
      .prefix = "dice_pairs"
    ),
    deployment = "main"
  ),
  tar_target(
    fit_g_full,
    fit_efa_g(
      indices_cogstruct,
      vars = names(indices_cogstruct),
      missing = "ml"
    )
  ),
  tar_target(
    comp_rel_g_full,
    tibble(comp_rel = unclass(semTools::compRelSEM(fit_g_full$nf1)))
  ),
  tar_target(
    scores_g_full,
    extract_g_scores(fit_g_full, data = indices_cogstruct)
  ),
  branches_g_chc
)
