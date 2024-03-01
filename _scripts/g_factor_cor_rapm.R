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

# https://github.com/ropensci/tarchetypes/issues/168
# adjust priority manually
for (i in seq_len(nrow(config_vars))) {
  for (target in c("vars_sample_batch", "vars_sample", "fit_g")) {
    branches_g[[target]][[i]]$settings$priority <- i / nrow(config_vars)
  }
}

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
    command = bind_rows_meta(
      !!!.x,
      .names = names(config_vars),
      .prefix = "cor_rapm"
    )
  ),
  tar_target(
    fit_g,
    fit_efa_g(
      indices_cogstruct,
      vars = setdiff(
        names(indices_cogstruct),
        match_game_index(game_id_reasoning)
      ),
      missing = "ml"
    )
  ),
  tar_target(
    scores_g,
    extract_g_scores(fit_g, data = indices_cogstruct)
  ),
  tar_target(
    cor_rapm_all,
    {
      subjs <- intersect(
        rownames(indices_cogstruct),
        rownames(indices_rapm)
      )
      cor(indices_rapm[subjs, ], scores_g[subjs, ], use = "pairwise")
    }
  )
)
