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
    path_obj_from_proj("subjs_keep_neural", "preproc_neural"),
    read = qs::qread(!!.x)
  ),
  tar_prep_files_cpm(),
  tarchetypes::tar_map(
    prepare_config_vars(n_vars_total, n_steps),
    tarchetypes::tar_rep(
      vars_sample,
      resample_vars(names(indices_cogstruct)[-1], num_vars, use_pairs),
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
        \(x) semTools::compRelSEM(x$nf1)
      ),
      fit_g,
      iteration = "list",
      deployment = "main"
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
    tarchetypes::tar_map(
      prepare_config_cpm(
        atlas == "Schaefer217",
        thresh_method == "alpha",
        thresh_level == 0.01
      ),
      names = !starts_with("file"),
      tarchetypes::tar_rep(
        cpm_result,
        lapply(
          list_flatten(scores_g),
          \(scores_list) {
            lapply_tar_batches(
              scores_list,
              perform_cpm_g_factor,
              file_fc, file_confounds, subjs_keep_neural,
              thresh_method, thresh_level,
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
        cpm_performance,
        lapply_tar_batches(
          cpm_result,
          \(result) {
            lapply_tar_batches(
              result,
              extract_cpm_performance,
              .append = TRUE
            ) |>
              list_rbind_tar_batches(names_to = "id_pairs")
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
      )
    )
  )
)
