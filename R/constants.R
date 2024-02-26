game_id_rapm <- bit64::as.integer64(265520726213317) # 瑞文高级推理
# data for this game should be removed because another version was used
game_id_rata <- bit64::as.integer64(240052750955077) # 远距离联想A (obsolete)

# used in data quality check
thresh_prop_miss <- 0.25
thresh_prop_guess <- 0.25
thresh_prop_invalid <- 0.2 # consistent with my CC paper

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
config_format <- readr::read_csv(
  "config/game_format.csv",
  col_types = readr::cols(game_id = "I")
)
config_data_correction <- readr::read_csv(
  "config/game_data_correction.csv",
  col_types = readr::cols(game_id = "I")
)

# used in cfa modeling building
hypers_config_dims <- tibble::tibble(
  name = c(
    "full", # all kept items after thinning
    "thresh_sil_050", # items with good silhouette (larger than 0.5)
    "thresh_sil_070", # items with strong silhouette (larger than 0.7)
    "top_sil_3", # top 3 biggest silhouette items
    "top_sil_4", # top 4 biggest silhouette items
    "thresh_load_030", # items with good loading (larger than 0.3)
    "thresh_load_040", # items with strong loading (larger than 0.4)
    "top_load_3", # top 3 biggest loading items
    "top_load_4", # top 4 biggest loading items
    "adjusted" # adjusted by removing biased tasks based on `"thresh_sil_05"`
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
tasks_biased <- c("NsymNCmp", "TOJ", "RP", "DRMA", "CardSortPro")

# used in functional connectivity settings
params_fmri_tasks <- tibble::tribble(
  ~session, ~task,
  "1", "rest",
  "1", "am",
  "1", "movie",
  "2", "rest",
  "2", "wm",
  "2", "movie"
)
params_xcpd <- tibble::tibble(
  config = c(
    "gsr", # with global signal regression
    "no_gsr" # no global signal regression
  )
)
params_atlas <- tibble::tibble(
  atlas = c(
    "Schaefer217",
    "4S256Parcels"
  )
)
config_fc <- tidyr::expand_grid(
  params_xcpd,
  params_fmri_tasks,
  params_atlas
) |>
  dplyr::filter(
    (config == "no_gsr" & atlas == "Schaefer217") |
      (config == "gsr" & atlas == "4S256Parcels")
  )

# used in CPM modeling building
hypers_cpm <- dplyr::bind_rows(
  tibble::tibble(
    thresh_method = "alpha",
    thresh_level = c(0.05, 0.01, 0.005, 0.001)
  ),
  tibble::tibble(
    thresh_method = "sparsity",
    thresh_level = c(0.01, 0.025, 0.05, 0.1)
  )
)
