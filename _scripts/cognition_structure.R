library(targets)
tar_source()
tar_option_set(
  packages = c("tidyverse", "bit64", "lavaan"),
  format = "qs",
  controller = setup_crew_controller("efa")
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
      indices_cogstruct_censor,
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
    indices_splitted,
    split_data_solomon(indices_cogstruct_games_censored)
  ),
  tar_target(
    efa_results,
    lapply(indices_splitted, iterate_efa)
  ),
  tar_target(
    efa_result_final,
    iterate_efa(indices_cogstruct_games_censored)
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
      inner_join(
        efa_result_final$efa |>
          parameters::model_parameters(threshold = "max") |>
          pivot_longer(
            starts_with("MR"),
            names_to = "efa",
            values_to = "loading",
            values_drop_na = TRUE
          ) |>
          select(efa, manifest = Variable, loading),
        by = "manifest"
      )
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
    extract_latent_scores(fit_bf_efa, indices_cogstruct_censor)
  )
)
