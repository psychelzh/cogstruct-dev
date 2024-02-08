library(targets)
tar_source()
tar_option_set(
  packages = c("tidyverse", "bit64", "lavaan"),
  format = "qs",
  controller = setup_crew_controller("efa")
)
setup_parallel_plan()

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
      select(indices_cogstruct, !user_id),
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
  ),
  tar_target(
    config,
    as_tibble(
      cluster_result$pamobject$silinfo$widths,
      rownames = "observed"
    ) |>
      mutate(
        latent = sprintf("F%d", cluster),
        # `NA` means free parameter
        fix = if_else(sil_width > 0.5, NA, 0)
      )
  ),
  tar_fit_cfa(
    config,
    indices_cogstruct,
    "fo",
    col_fix = fix,
    missing = "pairwise",
    tar_post_fit = "gof"
  )
)

model_comparison <- tarchetypes::tar_map(
  tidyr::expand_grid(
    schema = config_var_selection$schema,
    x = range_n_fact,
    y = range_n_fact
  ) |>
    dplyr::filter(x > y),
  tar_target(
    comparison,
    with(
      filter(fits, .data[["schema"]] == schema),
      possibly(
        nonnest2::vuongtest,
        quiet = FALSE
      )(
        fit[[which(n_fact == x)]],
        fit[[which(n_fact == y)]]
      )
    )
  )
)

list(
  tarchetypes::tar_file_read(
    indices_cogstruct,
    path_obj_from_proj("indices_cogstruct", "prepare_source_data"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_map(
    config_var_selection,
    names = !exclude,
    tar_target(
      n_factors_test,
      indices_cogstruct |>
        select(!user_id, !contains(exclude)) |>
        parameters::n_factors(rotation = "oblimin")
    )
  ),
  targets_fact_resamples,
  tarchetypes::tar_combine(
    cluster_stats,
    targets_fact_resamples$cluster_result,
    command = list(!!!.x) |>
      map(
        \(pk) {
          as_tibble(pk[c("nc", "crit")]) |>
            mutate(k = seq_len(n()), .before = 1L) |>
            filter(k %in% range_n_fact)
        }
      ) |>
      bind_rows(.id = ".id") |>
      zutils::separate_wider_dsv(
        ".id",
        c("schema", "n_fact"),
        prefix = "cluster_result"
      )
  ),
  tarchetypes::tar_combine(
    gofs,
    targets_fact_resamples$gof,
    command = list(!!!.x) |>
      map(\(x) as_tibble(x)) |>
      bind_rows(.id = ".id") |>
      zutils::separate_wider_dsv(
        ".id", c("schema", "n_fact"),
        prefix = "gof"
      )
  ),
  tarchetypes::tar_combine(
    fits,
    targets_fact_resamples$fit,
    command = list(!!!.x) |>
      map(\(x) tibble(fit = list(x))) |>
      bind_rows(.id = ".id") |>
      zutils::separate_wider_dsv(
        ".id", c("schema", "n_fact"),
        prefix = "fit"
      )
  ),
  model_comparison,
  tarchetypes::tar_combine(
    comparison,
    model_comparison$comparison,
    command = list(!!!.x) |>
      purrr::map(
        ~ if (inherits(.x, "vuongtest")) {
          tibble(
            omega = .x$omega,
            p_omega = .x$p_omega,
            p_left_better = .x$p_LRT$A,
            p_right_better = .x$p_LRT$B
          )
        }
      ) |>
      bind_rows(.id = ".id") |>
      zutils::separate_wider_dsv(
        ".id", c("schema", "left", "right"),
        prefix = "comparison"
      ),
    deployment = "main"
  )
)
