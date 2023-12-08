# Created by tarflow.iquizoo::use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c("tarflow.iquizoo", "tidyverse"),
  imports = "preproc.iquizoo",
  format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 8 workers which will run as local R processes:
  controller = crew::crew_controller_local(workers = 8)
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# 注意警觉, 注意指向: 1.0.0 records device for all right arrow resp as "mouse"
game_id_dev_err <- bit64::as.integer64(c(380173315257221, 380174783693701))
# 多彩文字PRO: correct game duration data
game_id_strp <- bit64::as.integer64(224378628399301)
# 人工词典: re-grade accuracy based on raters
game_id_cr <- bit64::as.integer64(380174879445893)
games_keyboard <- readr::read_lines("config/games_keyboard")
contents <- tarflow.iquizoo:::fetch_iquizoo_mem(
  readr::read_file("sql/contents_with_retest.sql")
)
config_contents <- contents |>
  dplyr::distinct(game_id) |>
  dplyr::inner_join(data.iquizoo::game_info, by = "game_id") |>
  dplyr::mutate(
    game_id = as.character(game_id),
    require_keyboard = game_name %in% games_keyboard,
    tar_parsed = rlang::syms(
      stringr::str_glue("raw_data_parsed_{game_id}")
    )
  )

# TODO: currently replicated codes, consider adding function
targets_valid_raw <- c(
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
targets_preproc <- tarflow.iquizoo:::tar_action_raw_data(
  contents |>
    dplyr::distinct(game_id) |>
    data.iquizoo::match_preproc(type = "semi", rm_tagged = TRUE),
  name_parsed = "data_valid",
  action_raw_data = "preproc"
)

targets_reliabilty <- tarchetypes::tar_map(
  values = contents |>
    dplyr::distinct(game_id) |>
    data.iquizoo::match_preproc(type = "semi", rm_tagged = TRUE) |>
    dplyr::left_join(data.iquizoo::game_info, by = "game_id") |>
    dplyr::mutate(
      game_id_rel = dplyr::coalesce(game_id_parallel, game_id)
    ) |>
    dplyr::summarise(
      tar_indices = rlang::syms(
        stringr::str_glue("indices_{game_id}")
      ) |>
        list(),
      .by = game_id_rel
    ) |>
    dplyr::mutate(game_id_rel = as.character(game_id_rel)),
  names = game_id_rel,
  tar_target(
    reliability,
    calc_reliability(bind_rows(tar_indices))
  )
)

# Replace the target list below with your own:
list(
  tarflow.iquizoo::tar_prep_iquizoo(
    contents = contents,
    what = "raw_data", # change to "scores" or "raw_data" if you want to
    action_raw_data = "parse",
    check_progress = FALSE # set as `FALSE` if projects finalized
  ),
  # more targets goes here
  tar_prep_creativity(),
  targets_valid_raw,
  targets_preproc,
  tarchetypes::tar_combine(indices, targets_preproc$indices),
  targets_reliabilty,
  tar_combine_with_meta(
    reliability,
    targets_reliabilty,
    cols_targets = "game_id",
    fun_post = \(.data) .data |> mutate(game_id = bit64::as.integer64(game_id))
  )
)
