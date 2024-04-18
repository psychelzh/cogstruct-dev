library(targets)
tar_source()
tar_option_set(
  packages = c("tidyverse", "bit64", "lavaan"),
  format = "qs",
  controller = setup_crew_controller("bsfa")
)
setup_parallel_plan()

targets_cfa <- tarchetypes::tar_map(
  list(type = rlang::syms(c("efa", "chc", "cognition"))),
  tar_target(
    config_dims,
    config_dims_alternatives |>
      select(manifest, latent = type)
  ),
  tarchetypes::tar_map(
    hypers_model,
    tar_fit_cfa(
      config_dims,
      indices_cogstruct_games_censored,
      theory = theory,
      missing = "ml",
      tar_post_fit = "gof"
    )
  )
)

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
    fspe::fspe(
      drop_na(indices_cogstruct_games_censored),
      14,
      nfold = 5,
      rep = 10
    )
  ),
  tar_target(file_kfa, "data/fa/kfa_ml.rds"),
  tar_target(
    fit_kfa,
    readRDS(file_kfa) |>
      kfa::k_model_fit(
        index = c("chisq", "df", "cfi", "tli", "rmsea", "srmr")
      ) |>
      list_rbind(names_to = "fold") |>
      mutate(n_factor = parse_number(model), .keep = "unused")
  ),
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
  tar_target(loadings_kept, trim_loadings(loadings_bootstrap)),
  tar_target(
    model_efa,
    loadings_kept |>
      apply(1, which.max) |>
      enframe(name = "manifest", value = "latent") |>
      mutate(latent = str_c("F", latent))
  ),
  tar_target(
    indices_splitted,
    indices_cogstruct_games_censored |>
      select(model_efa$manifest) |>
      split_data_solomon()
  ),
  tar_target(efa_solomon, psych::fa(indices_splitted[[1]], 7)),
  tar_target(
    cfa_solomon,
    fit_cfa(model_efa, indices_splitted[[2]], "fo", missing = "pairwise")
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
  ),
  tarchetypes::tar_file_read(
    config_dims_theory,
    "config/game_dims_theory.csv",
    read = read_csv(!!.x, col_types = cols(game_id = "I"))
  ),
  tar_target(
    config_dims_alternatives,
    config_dims_theory |>
      unite(manifest, game_name_abbr, index_name, sep = ".") |>
      inner_join(rename(model_efa, efa = latent), by = "manifest")
  ),
  tar_target(
    file_config_dims,
    config_dims_alternatives |>
      writexl::write_xlsx("_output/config_dims.xlsx"),
    format = "file"
  ),
  targets_cfa,
  tarchetypes::tar_combine(
    gofs,
    zutils::select_list(targets_cfa, starts_with("gof")),
    command = list(!!!.x) |>
      map(as_tibble) |>
      bind_rows(.id = ".id") |>
      zutils::separate_wider_dsv(
        ".id",
        c(names(hypers_model), "type"),
        patterns = c(".+?", ".+"),
        prefix = "gof"
      )
  ),
  tar_target(
    scores,
    extract_latent_scores(fit_bf_efa, indices_cogstruct_games_censored)
  )
)
