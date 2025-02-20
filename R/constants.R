game_id_unused <- bit64::as.integer64(
  c(
    240052750955077, # 远距离联想A (obsolete)
    668352655441989 # 短视频程序使用
  )
)
game_id_reasoning <- bit64::as.integer64(
  c(
    355740871500677,
    355740333933445,
    356101783560965,
    411281158706373,
    324179001000709,
    268008982659144,
    324179964613381,
    361750352892805,
    324177880433541
  )
)
# we define here to be used in g factor resampling
num_vars_total <- 77
game_index_dims <- readr::read_csv(
  "config/game_dims_theory.csv",
  show_col_types = FALSE
)

# used in data quality check
thresh_prop_miss <- 0.25
thresh_prop_guess <- 0.25
thresh_prop_invalid <- 0.2 # consistent with my CC paper
thresh_reliability <- 0.5 # consider low reliability

# used in raw data validation
game_id_keyboard <- readr::read_csv(
  "config/games_keyboard.csv",
  col_types = readr::cols(game_id = "I")
)$game_id
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
game_indices <- readr::read_csv(
  "config/game_indices.csv",
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
  ~session, ~task, ~runs,
  "1", "rest", 2,
  "1", "am", 2,
  "1", "movie", 2,
  "2", "rest", 2,
  "2", "wm", 3,
  "2", "movie", 2
) |>
  tidyr::uncount(runs, .id = "run_id")
# Note: sesion: 0 = all sessions, run_id: 0 = all runs
params_fmri_special <- dplyr::bind_rows(
  tibble::tibble(
    session = "0",
    task = "latent",
    run_id = c(0, 1)
  ),
  tibble::tibble(
    session = "0",
    task = c("rest", "movie"),
    run_id = 0
  )
)

params_conmat <- tidyr::expand_grid(
  xcpd = c(
    "gsr", # with global signal regression
    "no_gsr" # no global signal regression
  ),
  atlas = c(
    "4S256Parcels",
    # this is the same as 4S256Parcels but without subcortical regions
    "Schaefer200Parcels"
  )
) |>
  dplyr::filter(xcpd == "gsr")
config_fmri <- tidyr::expand_grid(
  params_fmri_tasks,
  params_conmat
)
config_fc <- dplyr::bind_rows(
  config_fmri,
  config_fmri |>
    dplyr::distinct(dplyr::pick(!run_id)) |>
    dplyr::mutate(
      run_id = ifelse(task == "wm", list(c(0, 12)), list(0))
    ) |>
    tidyr::unnest(run_id),
  tidyr::expand_grid(params_fmri_special, params_conmat)
) |>
  dplyr::mutate(
    run = ifelse(
      run_id == 0,
      "full",
      sprintf("run%d", run_id)
    ),
    .keep = "unused",
    # backward compatibility
    .after = dplyr::last_col()
  )
params_efficiency <- dplyr::bind_rows(
  tibble::tibble(
    weighted = TRUE,
    thresh_prop = 0,
    negatives = FALSE
  ),
  tidyr::expand_grid(
    weighted = FALSE,
    thresh_prop = seq(0.1, 0.5, 0.1),
    negatives = c(FALSE, TRUE)
  )
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
names_exclude <- c("file_fc", "fd", "file_atlas_dseg")
