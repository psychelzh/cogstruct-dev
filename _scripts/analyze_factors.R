library(targets)
tar_source()
tar_option_set(
  packages = c("tidyverse", "bit64", "lavaan"),
  format = "qs",
  controller = setup_crew_controller("bsfa")
)
setup_parallel_plan()

list(
  tarchetypes::tar_file_read(
    indices_cogstruct_games_censored,
    path_obj_from_proj(
      "indices_cogstruct_games_censored",
      "prepare_source_data"
    ),
    read = qs::qread(!!.x)
  ),
  tar_target(
    fspe,
    fspe::fspe(drop_na(indices_cogstruct_games_censored), 14, rep = 10)
  ),
  tar_target(file_kfa, "data/fa/kfa_ml.rds"),
  tar_target(
    target,
    readRDS(file_kfa)$cfa.syntax |>
      map_chr("7-factor") |>
      unique() |>
      lavaan::lavParseModelString() |>
      as_tibble() |>
      prepare_procrustes_target()
  ),
  tarchetypes::tar_rep(
    procrustes_loadings,
    get_procrusted_loadings(
      slice_sample(indices_cogstruct_games_censored, prop = 1, replace = TRUE),
      target
    ),
    batches = 100,
    reps = 10,
    iteration = "list"
  ),
  tar_target(
    loadings_bootstrap,
    do.call(
      abind::abind,
      c(
        map(list_flatten(procrustes_loadings), "loadingsPROC"),
        along = 3
      )
    )
  ),
  tar_target(loadings_base, trim_loadings(loadings_bootstrap)),
  tar_target(
    model_efa,
    loadings_base |>
      apply(1, which.max) |>
      enframe(name = "manifest", value = "latent") |>
      mutate(latent = str_c("F", latent))
  ),
  tarchetypes::tar_map(
    hypers_model,
    tarchetypes::tar_rep(
      fit_bootstrap,
      list(
        fit_cfa(
          model_efa,
          slice_sample(
            indices_cogstruct_games_censored,
            prop = 1, replace = TRUE
          ),
          theory = theory,
          missing = "pairwise"
        )
      ),
      batches = 10,
      reps = 10,
      iteration = "list"
    ),
    tarchetypes::tar_rep2(
      gof_bootstrap,
      lapply_tar_batches(
        fit_bootstrap,
        \(x) as_tibble_row(fitmeasures(x))
      ) |>
        list_rbind(),
      fit_bootstrap
    )
  )
)
