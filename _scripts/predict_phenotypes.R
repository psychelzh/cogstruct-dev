library(targets)
tar_source()
tar_option_set(
  packages = c("tidyverse", "bit64"),
  format = "qs",
  controller = setup_crew_controller("pred_pheno")
)
setup_parallel_plan()

cpm_branches <- tarchetypes::tar_map(
  config_cpm,
  names = !starts_with("file"),
  tarchetypes::tar_rep(
    cpm_result,
    apply(
      qs::qread(file_scores_factor)[subjs_to_keep, ], 2,
      \(scores) {
        cpmr::cpm(
          qs::qread(file_fc)[subjs_to_keep, ],
          scores,
          confounds = qs::qread(file_confounds)[subjs_to_keep, ],
          thresh_method = thresh_method,
          thresh_level = thresh_level,
          kfolds = 10
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
      extract_cpm_performance
    ) |>
      list_rbind(names_to = "latent"),
    cpm_result,
    retrieval = "worker",
    storage = "worker"
  )
)
list(
  tar_target(
    file_scores_factor,
    path_obj_from_proj("scores_bf_full", "confirm_factors"),
    format = "file_fast"
  ),
  tarchetypes::tar_file_read(
    subjs_keep_neural,
    path_obj_from_proj("subjs_keep_neural", "preproc_neural"),
    read = qs::qread(!!.x)
  ),
  tar_target(
    subjs_to_keep,
    # intersect() does not work for integer64
    # https://github.com/truecluster/bit64/issues/29
    intersect(
      as.character(subjs_keep_neural),
      rownames(qs::qread(file_scores_factor))
    )
  ),
  tar_prep_files_cpm(),
  cpm_branches,
  tarchetypes::tar_combine(
    cpm_performance,
    cpm_branches$cpm_performance,
    command = bind_rows_meta(
      !!!.x,
      .prefix = "cpm_performance",
      .type = "cpm"
    )
  )
)