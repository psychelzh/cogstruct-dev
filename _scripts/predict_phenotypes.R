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
    scores_factor, {
      tbl <- qs::qread(file_scores_factor)
      structure(
        as.matrix(select(tbl, !user_id)),
        id = tbl$user_id
      )
    }
  ),
  tar_target(dim_labels, colnames(scores_factor)),
  tar_target(
    subjs_to_keep, {
      # intersect() does not work for integer64
      # https://github.com/truecluster/bit64/issues/29
      subjs_neural <- qs::qread(file_subjs_keep_neural)
      subjs_behav <- attr(scores_factor, "id")
      matched <- match(subjs_neural, subjs_behav)
      subjs_behav[matched[!is.na(matched)]]
    }
  ),
  tar_prep_files_cpm(),
  tar_cpm_main(
    scores_factor,
    subjs_to_keep,
    combine = "cpm_performance"
  )
)
