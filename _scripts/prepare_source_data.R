library(targets)
future::plan(future.callr::callr)
tar_source()
tar_option_set(
  package = c("tidyverse", "bit64", "lavaan", "preproc.iquizoo"),
  format = "qs",
  imports = "preproc.iquizoo",
  memory = "transient",
  garbage_collection = TRUE,
  controller = crew::crew_controller_local(workers = 8)
)

read_archived <- function(...) {
  select(
    targets::tar_read(...),
    !contains("name")
  )
}

game_id_rapm <- bit64::as.integer64(265520726213317) # 瑞文高级推理
# 注意警觉, 注意指向: 1.0.0 records device for all right arrow resp as "mouse"
game_id_dev_err <- bit64::as.integer64(c(380173315257221, 380174783693701))
# 多彩文字PRO: correct game duration data
game_id_strp <- bit64::as.integer64(224378628399301)
# 人工词典: re-grade accuracy based on raters
game_id_cr <- bit64::as.integer64(380174879445893)
path_archive <- Sys.getenv("OneDriveConsumer") |>
  fs::path("Documents/Research/archived/cogstruct-dev-archived")
path_restore <- withr::with_dir(
  path_archive,
  fs::path(
    path_archive,
    tar_config_get("store", project = "preproc_behav")
  )
)
contents <- tarflow.iquizoo:::fetch_iquizoo_mem(
  readr::read_file("sql/contents_camp.sql")
)
games_keyboard <- readr::read_lines("config/games_keyboard")
targets_current <- tarflow.iquizoo::tar_prep_iquizoo(
  contents = contents,
  what = "raw_data",
  action_raw_data = "none",
  check_progress = FALSE
)
config_contents <- contents |>
  dplyr::distinct(game_id) |>
  dplyr::left_join(data.iquizoo::game_info, by = "game_id") |>
  dplyr::mutate(
    game_id = as.character(game_id),
    name_current = rlang::syms(stringr::str_glue("raw_data_{game_id}")),
    name_restore = rlang::syms(stringr::str_glue("data_{game_name_abbr}")),
    require_keyboard = game_name %in% games_keyboard,
    tar_parsed = rlang::syms(stringr::str_glue("data_parsed_{game_id}"))
  )
targets_main <- tarchetypes::tar_map(
  values = config_contents,
  names = game_id,
  tar_target(
    data_full,
    bind_rows(
      select(name_current, -project_id),
      possibly(read_archived)(name_restore, store = path_restore)
    ) |>
      distinct()
  ),
  tar_target(data_parsed, wrangle_data(data_full))
)
targets_valid_raw <- list(
  tarchetypes::tar_map(
    values = config_contents |>
      dplyr::filter(!game_id %in% c(game_id_dev_err, game_id_strp, game_id_cr)),
    names = game_id,
    tar_target(
      data_valid,
      validate_data(tar_parsed, require_keyboard)
    )
  ),
  # correct device error
  tarchetypes::tar_map(
    values = config_contents |>
      dplyr::filter(game_id %in% game_id_dev_err),
    names = game_id,
    tar_target(
      data_valid,
      correct_device(tar_parsed) |>
        validate_data(require_keyboard)
    )
  ),
  tarchetypes::tar_map(
    values = config_contents |>
      dplyr::filter(game_id %in% game_id_strp),
    names = game_id,
    tar_target(
      data_valid,
      validate_data(tar_parsed, require_keyboard) |>
        correct_game_dur()
    )
  ),
  # correct accuracy scores for CR
  tarchetypes::tar_map(
    values = config_contents |>
      dplyr::filter(game_id == game_id_cr),
    names = game_id,
    tar_target(
      data_valid,
      validate_data(tar_parsed, require_keyboard) |>
        correct_cr(cr_correction)
    )
  )
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
  targets_current,
  targets_main,
  targets_valid_raw,
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
