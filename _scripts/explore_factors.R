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

evaluate_best <- tarchetypes::tar_map(
  tibble::tibble(
    n = 1:10,
    name = sprintf("best_%d", n)
  ),
  names = name,
  tar_target(
    config,
    silinfo_best |>
      filter(row_number(desc(crit)) == n) |>
      pluck("sil", 1) |>
      filter(sil_width > 0.5) |>
      mutate(latent = sprintf("F%d", cluster))
  ),
  tar_fit_cfa(
    indices_wider_clean,
    config,
    col_manifest = game_index,
    col_latent = latent,
    theory = "fo"
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
      filter(k == nc) |>
      select("schema", "n_fact", "nc", "crit") |>
      left_join(cluster_result, by = c("schema", "n_fact")) |>
      mutate(
        sil = map(
          pk,
          ~ as_tibble(
            .x$pamobject$silinfo$widths,
            rownames = "game_index"
          )
        ),
        .keep = "unused"
      )
  ),
  evaluate_best,
  tarchetypes::tar_combine(
    results_best,
    evaluate_best$results,
    command = bind_rows(!!!.x, .id = ".id") |>
      zutils::separate_wider_dsv(
        ".id", "n",
        prefix = "results_best"
      )
  )
)
