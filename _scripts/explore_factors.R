library(targets)
future::plan(future.callr::callr)
tar_source()
tar_option_set(
  package = c("tidyverse", "bit64", "lavaan"),
  format = "qs",
  memory = "transient",
  garbage_collection = TRUE,
  controller = crew::crew_controller_local(workers = 8)
)

games_thin <- with(
  readr::read_tsv(
    "config/games_thin.tsv",
    show_col_types = FALSE
  ),
  sort(game_name_abbr[thin])
)

# parameters for resampling
n_resamples <- 1000
n_batches <- 10
n_reps <- n_resamples / n_batches

# configurations for factor analysis
config_var_selection <- tibble::tribble(
  ~schema, ~exclude,
  "all", character(),
  "thin", games_thin
)
range_n_fact <- 4:20

targets_fact_resamples <- tarchetypes::tar_map(
  tidyr::expand_grid(
    config_var_selection,
    n_fact = range_n_fact
  ),
  names = !exclude,
  tarchetypes::tar_rep(
    fact_attribution,
    resample_fact_attribution(
      indices_wider_clean,
      n_fact,
      exclude
    ),
    batches = n_batches,
    reps = n_reps
  ),
  tar_target(
    prob_one_fact,
    extract_prob_one_fact(fact_attribution)
  ),
  tar_target(
    cluster_result,
    # use pam clustering method to account for possible outliers
    fpc::pamk(
      as.dist(n_resamples - prob_one_fact),
      krange = range_n_fact
    )
  )
)

list(
  tarchetypes::tar_file_read(
    indices_wider_clean,
    path_obj_from_proj("indices_wider_clean", "prepare_source_data"),
    read = select(qs::qread(!!.x), !user_id)
  ),
  tarchetypes::tar_map(
    config_var_selection,
    names = !exclude,
    tar_target(
      n_factors_test,
      indices_wider_clean |>
        select(!contains(exclude)) |>
        parameters::n_factors(rotation = "oblimin")
    )
  ),
  targets_fact_resamples,
  tarchetypes::tar_combine(
    cluster_result,
    targets_fact_resamples$cluster_result,
    command = list(!!!.x) |>
      map(\(pk) tibble(pk = list(pk))) |>
      bind_rows(.id = ".id") |>
      zutils::separate_wider_dsv(
        ".id",
        c("schema", "n_fact"),
        prefix = "cluster_result"
      )
  ),
  tar_target(
    cluster_stats,
    cluster_result |>
      reframe(
        map(
          pk,
          ~ as_tibble(.x[c("nc", "crit")]) |>
            mutate(k = seq_len(n()), .before = 1L) |>
            filter(k %in% range_n_fact)
        ) |>
          list_rbind(),
        .by = c("schema", "n_fact")
      )
  ),
  tar_target(
    silinfo_best,
    cluster_stats |>
      slice_max(crit, by = c("schema", "n_fact")) |>
      slice_max(crit, by = "schema") |>
      left_join(cluster_result, by = c("schema", "n_fact")) |>
      mutate(
        sil = map(
          pk,
          ~ as_tibble(
            .x$pamobject$silinfo$widths,
            rownames = "game_index"
          )
        )
      ) |>
      pull(sil, name = "schema")
  ),
  tar_target(
    fil_silinfo_best,
    silinfo_best |>
      map(
        ~ .x |>
          mutate(game_index = replace_as_name_cn(game_index)) |>
          separate_wider_delim(
            game_index, ".",
            names = c("game_name", "index_name")
          )
      ) |>
      writexl::write_xlsx("config.local/silinfo.xlsx"),
    format = "file"
  )
)
