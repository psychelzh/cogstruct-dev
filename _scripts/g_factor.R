library(targets)
tar_option_set(
  packages = c("tidyverse", "bit64", "lavaan"),
  format = "qs",
  controller = if (Sys.info()["nodename"] == "shadow") {
    crew.cluster::crew_controller_sge(
      name = "gf",
      workers = 40,
      seconds_idle = 30
    )
  } else {
    crew::crew_controller_local(
      name = "gf-local",
      workers = 16,
      seconds_idle = 10
    )
  }
)
# tar_make_clustermq() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
options(clustermq.scheduler = "sge")
options(clustermq.template = "clustermq.tmpl")
tar_source()

n_vars_total <- 76
n_steps <- 20

list(
  tarchetypes::tar_file_read(
    indices_cogstruct,
    path_obj_from_proj("indices_cogstruct", "prepare_source_data"),
    read = qs::qread(!!.x)
  ),
  tar_target(
    indices_cogstruct_imp,
    # https://github.com/IQSS/Amelia/issues/47
    # use default, i.e., 5 times of imputations
    Amelia::amelia(as.data.frame(indices_cogstruct), idvars = "user_id")
  ),
  tarchetypes::tar_file_read(
    indices_rapm,
    path_obj_from_proj("indices_rapm", "prepare_source_data"),
    read = qs::qread(!!.x)
  ),
  tar_target(
    file_subjs_keep_neural,
    path_obj_from_proj("subjs_keep_neural", "preproc_neural"),
    format = "file_fast"
  ),
  tar_prep_files_cpm(),
  tarchetypes::tar_map(
    prepare_config_vars(n_vars_total, n_steps),
    tarchetypes::tar_rep(
      scores_g_imp,
      resample_g_scores_imp(
        indices_cogstruct_imp,
        num_vars,
        use_pairs
      ),
      batches = 10,
      reps = 10
    ),
    tarchetypes::tar_rep2(
      scores_g,
      scores_g_imp |>
        summarise(
          g = list(do.call(matsbyname::mean_byname, g)),
          .by = !c(impute, g)
        ),
      scores_g_imp
    ),
    tarchetypes::tar_map(
      config_files(),
      names = !starts_with("file"),
      tarchetypes::tar_rep(
        cpm_result,
        scores_g |>
          mutate(
            cpm_result = map(
              g,
              perform_cpm_g_factor,
              file_fc, file_confounds, file_subjs_keep_neural,
              thresh_method, thresh_level
            ),
            .keep = "unused"
          )
      )
    )
  )
)
