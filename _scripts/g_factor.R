library(targets)
tar_source()
tar_option_set(
  packages = c("tidyverse", "bit64", "lavaan"),
  format = "qs",
  controller = setup_crew_controller("gf")
)
setup_parallel_plan()

n_vars_total <- 76
n_steps <- 20

config_vars <- prepare_config_vars(n_vars_total, n_steps)
config_cpm <- prepare_config_cpm(
  config == "default",
  atlas == "Schaefer217",
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
          column_to_rownames("user_id") |>
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
    names = !starts_with("file"),
    tarchetypes::tar_rep(
      cpm_result,
      lapply(
        list_flatten(scores_g),
        \(scores_list) {
          lapply_tar_batches(
            scores_list,
            perform_cpm_g_factor,
            fc = qs::qread(file_fc),
            confounds = match_confounds(
              users_confounds,
              as.matrix(rowMeans(qs::qread(file_fd)))
            ),
            subjs_keep_neural = subjs_keep_neural,
            thresh_method = thresh_method,
            thresh_level = thresh_level,
            .append = TRUE
          )
        }
      ),
      batches = 4,
      reps = 5,
      iteration = "list",
      retrieval = "worker",
      storage = "worker"
    ),
    tarchetypes::tar_rep2(
      cpm_performance_cv,
      lapply_tar_batches(
        cpm_result,
        \(result) {
          lapply_tar_batches(
            result,
            extract_cpm_performance,
            .append = TRUE
          ) |>
            list_rbind_tar_batches(
              names_to = "id_pairs",
              append = TRUE
            )
        }
      ) |>
        list_rbind() |>
        # rename batch info from resample step to avoid conflict
        rename_with(
          \(x) sprintf("%s_resample", x),
          starts_with("tar")
        ),
      cpm_result,
      retrieval = "worker",
      storage = "worker"
    ),
    tar_target(
      cpm_performance,
      summarise(
        cpm_performance_cv,
        r = mean(r),
        .by = c(include, starts_with("tar"))
      )
    )
  )
)

targets_cpm_full <- tarchetypes::tar_map(
  config_cpm,
  names = !starts_with("file"),
  tarchetypes::tar_rep(
    cpm_result_full,
    perform_cpm_g_factor(
      qs::qread(file_fc),
      scores_g_full,
      match_confounds(
        users_confounds,
        as.matrix(rowMeans(qs::qread(file_fd)))
      ),
      subjs_keep_neural,
      thresh_method = thresh_method,
      thresh_level = thresh_level
    ),
    batches = 4,
    reps = 5,
    iteration = "list",
    retrieval = "worker",
    storage = "worker"
  ),
  tarchetypes::tar_rep2(
    cpm_performance_cv_full,
    extract_cpm_performance(cpm_result_full),
    cpm_result_full,
    retrieval = "worker",
    storage = "worker"
  ),
  tar_target(
    cpm_performance_full,
    summarise(cpm_performance_cv_full, r = mean(r), .by = include)
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
  tarchetypes::tar_file_read(
    subjs_keep_neural,
    path_obj_from_proj("subjs_keep_neural", "prepare_neural"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    users_confounds,
    path_obj_from_proj("users_confounds", "prepare_source_data"),
    read = qs::qread(!!.x)
  ),
  tar_prep_files_cpm(),
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
    command = list(!!!.x) |>
      lapply(bind_rows) |>
      bind_rows_meta(
        .names = c(
          names(select(config_cpm, !starts_with("file"))),
          names(config_vars)
        ),
        .prefix = "cpm_performance"
      )
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
  targets_cpm_full,
  tarchetypes::tar_combine(
    cpm_performance_full,
    targets_cpm_full$cpm_performance_full,
    command = bind_rows_meta(
      !!!.x,
      .names = names(select(config_cpm, !starts_with("file"))),
      .prefix = "cpm_performance_full"
    )
  )
)
