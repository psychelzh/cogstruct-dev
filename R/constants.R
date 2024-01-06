# special column names
col_raw_parsed <- "raw_parsed"

# games require special correction
game_id_rapm <- bit64::as.integer64(265520726213317) # 瑞文高级推理
game_id_cor <- list(
  # 注意警觉, 注意指向: 1.0.0 records device for all right arrow resp as "mouse"
  dev_err = bit64::as.integer64(c(380173315257221, 380174783693701)),
  # 多彩文字PRO: correct game duration data
  dur_err = bit64::as.integer64(224378628399301),
  # 人工词典: re-grade accuracy based on raters
  cr = bit64::as.integer64(380174879445893),
  # 图片记忆: keep test phase data only
  mst = bit64::as.integer64(c(268008982671439, 268008982671433)),
  rt = bit64::as.integer64(
    c(224379118576069, # 太空飞船PRO：reaction time should be added 100 ms
      268008982667347) # 捉虫高级简版：reaction time should be added 300 ms
  )
)

# used in users' motivation check
thresh_prop_miss <- 0.25
thresh_prop_guess <- 0.25

# used in raw data validation
games_keyboard <- readr::read_lines("config/games_keyboard")
game_data_names <- readr::read_csv(
  "config/game_data_names.csv",
  col_types = readr::cols(game_id = "I")
) |>
  dplyr::select(!game_name) |>
  dplyr::mutate(
    list_names = purrr::map(col_names, ~ eval(parse(text = .x))),
    .keep = "unused"
  ) |>
  tidyr::chop(list_names)

# used in cfa modeling building
hypers_config_dims <- tibble::tibble(
  name = c("full", "bigsil", "top4")
)
hypers_model_type <- tibble::tibble(
  hierarchical = c("none", "bifactor", "highorder")
)
