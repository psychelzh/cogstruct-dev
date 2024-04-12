library(targets)
tar_source()
tar_option_set(
  packages = c("tidyverse", "bit64", "lavaan"),
  format = "qs",
  controller = setup_crew_controller("gf")
)
setup_parallel_plan()

config_neural <- prepare_config_neural(
  xcpd == "gsr",
  task == "wm",
  run == "full",
  atlas == "4S256Parcels"
)
hypers_cpm <- hypers_cpm |>
  dplyr::filter(
    thresh_method == "alpha",
    thresh_level == 0.01
  )

config_vars <- prepare_config_vars(num_vars_total)
branches_g <- tarchetypes::tar_map(
  config_vars,
  tar_calibrate_g(
    resample_vars(names(indices_cogstruct), num_vars, use_pairs),
    indices_cogstruct,
    use_pairs,
    data_rapm = indices_rapm,
    config_neural = config_neural,
    hypers_cpm = hypers_cpm,
    batches = 10,
    reps = 10
  )
)

config_vars_no_rsn <- prepare_config_vars(
  num_vars_total - length(game_id_reasoning),
  use_pairs = FALSE
)
branches_g_no_rsn <- tarchetypes::tar_map(
  config_vars_no_rsn,
  tar_calibrate_g(
    resample_vars(
      setdiff(names(indices_cogstruct), match_game_index(game_id_reasoning)),
      num_vars
    ),
    indices_cogstruct,
    use_pairs,
    name_suffix = "no_rsn",
    data_rapm = indices_rapm,
    batches = 10,
    reps = 10
  )
)

config_vars_chc <- prepare_config_vars_chc(10, 1)
branches_g_chc <- tarchetypes::tar_map(
  config_vars_chc,
  names = !vars_pair,
  tar_calibrate_g(
    lapply(vars_pair, sample, num_vars),
    indices_cogstruct,
    use_pairs = TRUE,
    name_suffix = "chc",
    config_neural = config_neural,
    hypers_cpm = hypers_cpm,
    batches = 10,
    reps = 10
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
  tar_prepare_neural_data(config_neural),
  tar_calibrate_g(
    list(names(indices_cogstruct)),
    indices_cogstruct,
    use_pairs = FALSE,
    name_suffix = "full",
    data_rapm = indices_rapm,
    config_neural = config_neural,
    hypers_cpm = hypers_cpm
  ),
  branches_g,
  tarchetypes::tar_combine(
    rel_pairs_g,
    branches_g$rel_pairs_g,
    command = bind_rows_meta(
      !!!.x,
      .names = names(config_vars),
      .prefix = "rel_pairs_g"
    ),
    deployment = "main"
  ),
  tarchetypes::tar_combine(
    comp_rel_g,
    branches_g$comp_rel_g,
    command = list(!!!.x) |>
      lapply(bind_rows) |>
      bind_rows_meta(
        .names = names(config_vars),
        .prefix = "comp_rel_g"
      ),
    deployment = "main"
  ),
  tarchetypes::tar_combine(
    cor_rapm,
    branches_g$cor_rapm,
    command = list(!!!.x) |>
      lapply(bind_rows) |>
      bind_rows_meta(
        .names = names(config_vars),
        .prefix = "cor_rapm"
      ),
    deployment = "main"
  ),
  tarchetypes::tar_combine(
    cpm_performance,
    zutils::select_list(branches_g, starts_with("cpm_performance")),
    command = bind_rows_meta(
      !!!.x,
      .names = c(names(config_fc), names(hypers_cpm), names(config_vars)),
      .prefix = "cpm_performance"
    ),
    deployment = "main"
  ),
  tarchetypes::tar_combine(
    dice_pairs,
    zutils::select_list(branches_g, starts_with("dice_pairs")),
    command = bind_rows_meta(
      !!!.x,
      .names = c(names(config_fc), names(hypers_cpm), names(config_vars)),
      .prefix = "dice_pairs"
    ),
    deployment = "main"
  ),
  tar_calibrate_g(
    list(
      setdiff(names(indices_cogstruct), match_game_index(game_id_reasoning))
    ),
    indices_cogstruct,
    use_pairs = FALSE,
    name_suffix = "no_rsn_full",
    data_rapm = indices_rapm
  ),
  branches_g_no_rsn,
  tarchetypes::tar_combine(
    cor_rapm_no_rsn,
    branches_g_no_rsn$cor_rapm_no_rsn,
    command = bind_rows_meta(
      !!!.x,
      .names = names(config_vars_no_rsn),
      .prefix = "cor_rapm_no_rsn"
    )
  ),
  branches_g_chc
)
