library(targets)
tar_option_set(
  packages = c("tidyverse", "bit64"),
  format = "qs",
  controller = if (Sys.info()["nodename"] == "shadow") {
    crew.cluster::crew_controller_sge(
      name = "cpm",
      workers = 40,
      seconds_idle = 30
    )
  } else {
    crew::crew_controller_local(
      name = "cpm-local",
      workers = 16,
      seconds_idle = 10
    )
  }
)
tar_source()

cpm_branches <- tarchetypes::tar_map(
  config_files(),
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
    iteration = "list"
  ),
  tarchetypes::tar_rep2(
    cpm_performance,
    aggregate_performance(cpm_result),
    cpm_result
  )
)

list(
  tar_target(
    file_scores_factor,
    path_obj_from_proj("scores_bf_full", "confirm_factors"),
    format = "file_fast"
  ),
  tar_target(
    file_subjs_keep_neural,
    path_obj_from_proj("subjs_keep_neural", "preproc_neural"),
    format = "file_fast"
  ),
  tar_target(
    subjs_to_keep,
    # intersect() does not work for integer64
    # https://github.com/truecluster/bit64/issues/29
    intersect(
      as.character(qs::qread(file_subjs_keep_neural)),
      rownames(qs::qread(file_scores_factor))
    )
  ),
  tar_prep_files_cpm(),
  cpm_branches,
  tarchetypes::tar_combine(
    cpm_performance,
    cpm_branches$cpm_performance,
    command = bind_rows(!!!.x, .id = ".id") |>
      zutils::separate_wider_dsv(
        ".id",
        c(names(params_fmri_tasks), names(params_xcpd), names(hypers_cpm)),
        patterns = c(rep(".+?", 2), ".+", rep(".+?", 3)),
        prefix = "cpm_performance"
      )
  )
)
