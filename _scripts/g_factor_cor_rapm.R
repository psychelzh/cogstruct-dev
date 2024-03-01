library(targets)
tar_source()
tar_option_set(
  packages = c("tidyverse", "bit64", "lavaan"),
  format = "qs",
  controller = setup_crew_controller("gf")
)
setup_parallel_plan()

config_vars <- prepare_config_vars(
  num_vars_total - length(game_id_reasoning),
  use_pairs = FALSE
)
config_cpm <- prepare_config_cpm(
  config == "gsr",
  task == "wm",
  run == "full",
  thresh_method == "alpha",
  thresh_level == 0.01
)
branches_g <- tarchetypes::tar_map(
  config_vars,
  tarchetypes::tar_rep(
    vars_sample,
    resample_vars(
      setdiff(
        names(indices_cogstruct),
        match_game_index(game_id_reasoning)
      ),
      num_vars
    ),
    batches = 10,
    reps = 10,
    iteration = "list",
    deployment = "main"
  ),
  tarchetypes::tar_rep2(
    fit_g,
    lapply_tar_batches(
      vars_sample,
      fit_efa_g,
      data = indices_cogstruct,
      missing = "ml"
    ),
    vars_sample,
    iteration = "list"
  ),
  tarchetypes::tar_rep2(
    scores_g,
    lapply_tar_batches(
      fit_g,
      extract_g_scores,
      data = indices_cogstruct
    ),
    fit_g,
    iteration = "list"
  ),
  tarchetypes::tar_rep2(
    cor_rapm,
    lapply_tar_batches(
      scores_g,
      \(x) {
        indices_rapm |>
          merge(x, by = "row.names") |>
          summarise(r = cor(score, f1, use = "pairwise"))
      }
    ) |>
      list_rbind_tar_batches(),
    scores_g,
    iteration = "list"
  )
)

list(
  tarchetypes::tar_file_read(
    indices_cogstruct,
    path_obj_from_proj("indices_cogstruct", "prepare_source_data"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    indices_rapm,
    path_obj_from_proj("indices_rapm", "prepare_source_data"),
    read = qs::qread(!!.x)
  ),
  branches_g,
  tarchetypes::tar_combine(
    cor_rapm,
    branches_g$cor_rapm,
    command = list(!!!.x) |>
      lapply(bind_rows) |>
      bind_rows_meta(
        .names = names(config_vars),
        .prefix = "cor_rapm"
      )
  )
)
