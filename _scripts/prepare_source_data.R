library(targets)
future::plan(future.callr::callr)
tar_source()
tar_option_set(
  package = c("tidyverse", "bit64", "lavaan", "preproc.iquizoo"),
  format = "qs",
  imports = "preproc.iquizoo",
  memory = "transient",
  garbage_collection = TRUE,
  controller = crew::crew_controller_local(workers = 12)
)

contents <- tarflow.iquizoo:::fetch_iquizoo_mem(
  readr::read_file("sql/contents_camp.sql")
)

targets_preproc <- tarchetypes::tar_map(
  values = contents |>
    dplyr::distinct(game_id) |>
    data.iquizoo::match_preproc(
      type = "inner",
      rm_tagged = TRUE # the tagged are experimental or unavailable
    ) |>
    dplyr::mutate(
      game_id = as.character(.data$game_id),
      tar_parsed = rlang::syms(stringr::str_glue("data_valid_{game_id}"))
    ),
  names = game_id,
  tar_target(
    indices,
    preproc_data(tar_parsed, prep_fun, .input = input, .extra = extra),
    packages = c("tarflow.iquizoo", "preproc.iquizoo")
  ),
  tar_target(
    durations,
    tar_parsed |>
      mutate(game_dur_mins = game_duration / 60000) |>
      group_by(.data[["game_id"]]) |>
      skimr::skim(game_dur_mins) |>
      ungroup()
  )
)

list(
  tarflow.iquizoo::tar_prep_iquizoo(
    contents = contents,
    what = "raw_data",
    action_raw_data = "none",
    check_progress = FALSE
  ),
  tar_collect_camp(contents),
  tar_validate_rawdata(contents),
  targets_preproc,
  tarchetypes::tar_combine(indices, targets_preproc$indices),
  tarchetypes::tar_combine(durations, targets_preproc$durations),
  tar_prep_creativity(),
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
    indices_clean,
    clean_indices(indices, users_completed)
  ),
  tar_target(
    indices_of_interest,
    indices_clean |>
      inner_join(
        data.iquizoo::game_indices,
        join_by(game_id, index_name == index_main)
      ) |>
      mutate(score_adj = if_else(index_reverse, -score, score)) |>
      mutate(
        is_outlier_iqr = score %in% boxplot.stats(score)$out,
        .by = c(game_id, index_name)
      )
  ),
  tar_target(
    indices_wider_clean,
    indices_of_interest |>
      filter(
        !is_outlier_iqr,
        # RAPM test is not included in the factor analysis
        game_id != game_id_rapm
      ) |>
      mutate(game_index = str_c(game_name_abbr, index_name, sep = ".")) |>
      pivot_wider(
        id_cols = user_id,
        names_from = game_index,
        values_from = score_adj
      )
  ),
  tar_target(
    indices_rapm,
    indices_of_interest |>
      filter(game_id == game_id_rapm) |>
      select(user_id, index_name, score = score_adj)
  )
)
