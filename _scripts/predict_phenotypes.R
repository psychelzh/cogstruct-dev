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

targets_cpm <- tarchetypes::tar_map(
  params_fmri_tasks,
  tar_target(
    file_confounds,
    path_obj_from_proj(
      paste(
        "confounds_cpm",
        session, task,
        sep = "_"
      ),
      "preproc_neural"
    ),
    format = "file_fast"
  ),
  tarchetypes::tar_map(
    params_xcpd,
    tar_target(
      file_fc,
      path_obj_from_proj(
        paste(
          "fc_orig_full",
          config, session, task, atlas,
          sep = "_"
        ),
        "preproc_neural"
      ),
      format = "file_fast"
    ),
    tarchetypes::tar_map(
      hypers_cpm,
      tar_target(
        cpm_result,
        cpmr::cpm(
          match_cases(qs::qread(file_fc), subjs_to_keep),
          match_cases(scores_factor, subjs_to_keep)[, dim_labels],
          confounds = match_cases(qs::qread(file_confounds), subjs_to_keep),
          thresh_method = thresh_method,
          thresh_level = thresh_level,
          kfolds = 10
        ),
        pattern = map(dim_labels),
        iteration = "list"
      ),
      tar_target(
        cpm_performance,
        aggregate_performance(cpm_result, dim_labels)
      )
    )
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
  targets_cpm,
  tarchetypes::tar_combine(
    cpm_performance,
    zutils::select_list(targets_cpm, starts_with("cpm_performance")),
    command = bind_rows(!!!.x, .id = ".id") |>
      zutils::separate_wider_dsv(
        ".id",
        c(names(hypers_cpm), names(params_xcpd), names(params_fmri_tasks)),
        patterns = c(rep(".+?", 2), ".+_?.+", rep(".+?", 3)),
        prefix = "cpm_performance"
      )
  )
)
