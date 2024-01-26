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
  tarchetypes::tar_map(
    prepare_config_vars(n_vars_total, n_steps),
    tarchetypes::tar_rep(
      scores_g_imp,
      lapply(
        indices_cogstruct_imp$imputations,
        resample_g_scores,
        num_vars,
        use_pairs
      ) |>
        list_rbind(names_to = "impute"),
      batches = 10,
      reps = 10
    ),
    tarchetypes::tar_rep2(
      scores_g,
      scores_g_imp |>
        summarise(
          g = matsbyname::mean_byname(g),
          .by = !c(impute, g)
        ),
      scores_g_imp
    )
  )
)
