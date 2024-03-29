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
  tar_target(
    file_games_censor,
    "config/games_censor.txt",
    format = "file"
  ),
  tarchetypes::tar_file_read(
    indices_cogstruct,
    path_obj_from_proj("indices_cogstruct", "prepare_source_data"),
    read = qs::qread(!!.x)
  ),
  tar_target(
    config_games_censor,
    read_tsv(file_games_censor, col_types = cols(game_id = "I")) |>
      unite("game_index", game_name_abbr, index_name, sep = ".") |>
      left_join(
        enframe(psych::smc(indices_cogstruct), "game_index", "smc"),
        by = "game_index"
      ) |>
      arrange(paradigm_censor, desc(smc)) |>
      filter(!is.na(paradigm_censor)) |>
      mutate(censor = row_number() > 1, .by = paradigm_censor)
  ),
  tar_target(
    indices_cogstruct_censor,
    select(indices_cogstruct, !with(config_games_censor, game_index[censor]))
  ),
  tar_target(
    indices_splitted,
    split_data_solomon(indices_cogstruct_censor)
  ),
  tar_target(
    efa_results,
    lapply(indices_splitted, iterate_efa)
  ),
  tar_target(
    efa_result_final,
    iterate_efa(indices_cogstruct_censor)
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
      mutate(variable = match_name_cn_short(manifest)) |>
      writexl::write_xlsx("_output/config_dims.xlsx"),
    format = "file"
  ),
  targets_cfa,
  tarchetypes::tar_combine(
    gofs,
    zutils::select_list(targets_cfa, starts_with("gof")),
    command = list(!!!.x) |>
      map(\(x) as_tibble(x)) |>
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
