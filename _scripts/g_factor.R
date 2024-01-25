library(targets)
tar_option_set(
  packages = c("tidyverse", "lavaan"),
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

# number of different sample sizes
n_steps <- 20
# used in batched runs
n_batches <- 10
n_reps <- 10

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
  tar_target(batches, seq_len(n_batches)),
  tar_target(
    config_vars,
    prepare_config_vars(
      ncol(indices_cogstruct) - 1,
      n_steps
    )
  ),
  tar_target(
    scores_g,
    replicate(
      n_reps,
      with(
        config_vars,
        lapply(
          indices_cogstruct_imp$imputations,
          resample_g_scores,
          num_vars,
          use_pairs
        ) |>
          list_rbind(names_to = "impute")
      ),
      simplify = FALSE
    ) |>
      list_rbind(names_to = "rep") |>
      mutate(batch = batches, .before = 1L),
    pattern = cross(batches, config_vars)
  )
)
