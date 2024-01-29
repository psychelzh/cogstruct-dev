library(targets)
tar_source()
tar_option_set(
  packages = c("tidyverse", "bit64"),
  format = "qs",
  controller = setup_crew_controller("bench_cpm")
)
setup_parallel_plan()

list(
  tarchetypes::tar_file_read(
    scores_rapm,
    path_obj_from_proj("indices_rapm", "prepare_source_data"),
    read = qs::qread(!!.x) |>
      column_to_rownames("user_id") |>
      as.matrix()
  ),
  tarchetypes::tar_file_read(
    subjs_keep_neural,
    path_obj_from_proj("subjs_keep_neural", "preproc_neural"),
    read = qs::qread(!!.x)
  ),
  tar_target(
    subjs_to_keep,
    intersect(
      as.character(subjs_keep_neural),
      rownames(scores_rapm)
    )
  ),
  tar_prep_files_cpm(),
  tar_cpm_main(
    list(
      rapm = cpmr::cpm(
        qs::qread(file_fc)[subjs_to_keep, ],
        scores_rapm[subjs_to_keep, ],
        confounds = qs::qread(file_confounds)[subjs_to_keep, ],
        thresh_method = thresh_method,
        thresh_level = thresh_level,
        kfolds = 10
      )
    )
  )
)
