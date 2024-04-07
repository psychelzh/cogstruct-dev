library(targets)
tar_source()
tar_option_set(
  packages = c("tidyverse", "bit64", "lavaan", "preproc.iquizoo"),
  format = "qs",
  imports = "preproc.iquizoo",
  memory = "transient",
  garbage_collection = TRUE,
  controller = crew::crew_controller_local(workers = 12)
)

contents <- tarflow.iquizoo::fetch_iquizoo_mem()(
  readr::read_file("sql/contents_camp.sql")
) |>
  dplyr::filter(game_id != game_id_rata)

targets_preproc <- tarchetypes::tar_map(
  values = contents |>
    dplyr::distinct(game_id) |>
    data.iquizoo::merge_preproc(
      rm_tagged = TRUE # the tagged are experimental or unavailable
    ) |>
    dplyr::mutate(
      game_id = as.character(.data$game_id),
      tar_parsed = rlang::syms(stringr::str_glue("data_valid_{game_id}"))
    ),
  names = game_id,
  tar_target(
    indices,
    preproc_data(tar_parsed, prep_fun, .input = input, .extra = extra)
  ),
  tar_target(
    durations,
    tar_parsed |>
      mutate(
        game_dur_mins = game_duration / 60000 +
          case_match(
            .data[["game_id"]],
            bit64::as.integer64(c(268008982671439, 268008982671433)) ~ 4.7,
            bit64::as.integer64(c(268008982655069, 268008982667346)) ~ 2.7,
            bit64::as.integer64(324182449562501) ~ 8,
            .default = 0
          )
      ) |>
      group_by(.data[["game_id"]]) |>
      skimr::skim(game_dur_mins) |>
      ungroup()
  )
)

targets_check_motivated <- tar_check_motivated(
  config = readr::read_csv(
    "config/rules_unmotivated.csv",
    col_types = readr::cols(game_id = "I")
  )
)

targets_indices_partitioned <- tar_partition_rawdata(contents)

list(
  tarflow.iquizoo::tar_prep_iquizoo(
    contents = contents,
    what = "raw_data",
    action_raw_data = "none",
    check_progress = FALSE
  ),
  tar_collect_camp(contents),
  tar_validate_rawdata(contents),
  targets_check_motivated,
  tarchetypes::tar_combine(
    res_motivated,
    targets_check_motivated$res_motivated
  ),
  targets_preproc,
  tarchetypes::tar_combine(indices, targets_preproc$indices),
  tarchetypes::tar_combine(durations, targets_preproc$durations),
  if (FALSE) tar_prep_creativity(),
  tarchetypes::tar_file_read(
    users_project_progress,
    "sql/progress.tmpl.sql",
    read = tarflow.iquizoo::fetch_iquizoo(
      read_file(!!.x),
      params = list(unique(contents_origin$project_id))
    )
  ),
  tar_target(
    users_completed,
    users_project_progress |>
      filter(str_detect(project_name, "^认知实验[A-E]$")) |>
      summarise(n = sum(project_progress) / 100, .by = user_id) |>
      filter(n >= 4)
  ),
  tar_target(
    users_demography,
    users |>
      semi_join(users_completed, by = "user_id") |>
      prepare_users_demography(indices)
  ),
  tar_target(
    users_confounds,
    users_demography |>
      select(user_id, user_sex, user_age) |>
      mutate(
        scanner = names(data.camp::users_id_mapping)[
          match(user_id, data.camp::users_id_mapping)
        ] |>
          str_remove_all("\\d")
      ) |>
      drop_na() |>
      fastDummies::dummy_columns(
        "scanner",
        remove_first_dummy = TRUE,
        remove_selected_columns = TRUE
      ) |>
      column_to_rownames("user_id") |>
      as.matrix()
  ),
  tar_target(
    indices_cogstruct_long,
    censor_indices(
      indices,
      # RAMP is not included in structure exploration
      game_id != game_id_rapm,
      users_completed,
      res_motivated
    )
  ),
  tar_target(users_clean, screen_users(indices_cogstruct_long)),
  tar_target(
    indices_cogstruct,
    reshape_indices(indices_cogstruct_long, users_clean)
  ),
  tar_target(
    indices_rapm,
    indices |>
      censor_indices(
        game_id == game_id_rapm,
        users_completed,
        res_motivated
      ) |>
      filter(!is_outlier_iqr & is_motivated) |>
      select(user_id, score = score_adj) |>
      column_to_rownames("user_id") |>
      as.matrix()
  ),
  targets_indices_partitioned,
  tarchetypes::tar_combine(
    indices_slices,
    targets_indices_partitioned
  ),
  tar_target(
    indices_slices_cogstruct_long,
    censor_indices(
      indices_slices,
      game_id != game_id_rapm,
      users_completed,
      res_motivated,
      id_cols_extra = part
    )
  ),
  tar_target(
    indices_cogstruct_pool,
    bind_rows(
      indices_slices_cogstruct_long,
      add_column(indices_cogstruct_long, part = 1)
    )
  )
)
