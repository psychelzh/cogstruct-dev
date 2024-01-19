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
    c(
      224379118576069, # 太空飞船PRO：reaction time should be added 100 ms
      268008982667347 # 捉虫高级简版：reaction time should be added 300 ms
    )
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
  name = c(
    "full", # all kept items after thinning
    "good_sil", # items with good silhouette (larger than 0.5)
    "top_sil_3", # top 3 biggest silhouette items
    "top_sil_4", # top 4 biggest silhouette items
    "good_load", # items with good loading (larger than 0.4)
    "top_load_3", # top 3 biggest loading items
    "top_load_4", # top 4 biggest loading items
    "adjusted" # adjusted by removing biased tasks based on `"good_sil"`
  )
)
hypers_model <- tibble::tibble(
  theory = c(
    "of", # one-factor
    "fo", # first-order
    "ho", # higher-order
    "bf" # bifactor
  )
)
tasks_biased <- c("NsymNCmp", "TOJ", "RP", "DRMA")

# used in functional connectivity settings
hypers_xcpd_config <- tibble::tibble(
  config = c(
    "default", # with global signal regression
    "no_gsr" # no global signal regression
  )
)
hypers_fmri_dataset <- tibble::tribble(
  ~session, ~task,
  "1", "rest",
  "1", "am",
  "1", "movie",
  "2", "rest",
  "2", "wm",
  "2", "movie"
)
hypers_atlas <- tibble::tibble(
  atlas = sprintf("Schaefer%d17", 1:4)
)
