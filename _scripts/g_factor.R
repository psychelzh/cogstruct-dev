library(targets)
tar_source()
tar_option_set(
  packages = c("tidyverse", "bit64", "lavaan"),
  format = "qs",
  controller = setup_crew_controller("gf")
)
setup_parallel_plan()

config_vars <- prepare_config_vars(num_vars_total)
config_cpm <- prepare_config_cpm(
  config == "gsr",
  task == "wm",
  run == "full",
  thresh_method == "alpha",
  thresh_level == 0.01
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
    config_cpm,
    names = !c(file_fc, fd),
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
  tar_prepare_cpm(),
  branches_g,
  tarchetypes::tar_combine(
    rel_pairs_g,
    branches_g$rel_pairs_g,
    command = list(!!!.x) |>
      lapply(bind_rows) |>
      bind_rows_meta(
        .names = names(config_vars),
        .prefix = "rel_pairs_g"
      )
  ),
  tarchetypes::tar_combine(
    comp_rel_g,
    branches_g$comp_rel_g,
    command = list(!!!.x) |>
      lapply(bind_rows) |>
      bind_rows_meta(
        .names = names(config_vars),
        .prefix = "comp_rel_g"
      )
  ),
  tarchetypes::tar_combine(
    cor_rapm,
    branches_g$cor_rapm,
    command = list(!!!.x) |>
      lapply(bind_rows) |>
      bind_rows_meta(
        .names = names(config_vars),
        .prefix = "cor_rapm"
      )
  ),
  tarchetypes::tar_combine(
    cpm_performance,
    zutils::select_list(branches_g, starts_with("cpm_performance")),
    command = bind_rows_meta(
      !!!.x,
      .names = c(names(config_fc), names(hypers_cpm), names(config_vars)),
      .prefix = "cpm_performance"
    )
  ),
  tarchetypes::tar_combine(
    dice_pairs,
    zutils::select_list(branches_g, starts_with("dice_pairs")),
    command = bind_rows_meta(
      !!!.x,
      .names = c(names(config_fc), names(hypers_cpm), names(config_vars)),
      .prefix = "dice_pairs"
    )
  )
)
