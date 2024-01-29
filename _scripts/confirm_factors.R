library(targets)
tar_source()
tar_option_set(
  packages = c("tidyverse", "bit64", "lavaan"),
  format = "qs",
  controller = setup_crew_controller("cfa")
)
setup_parallel_plan()

prepare_config <- function(name, config, loadings = NULL) {
  if (!is.null(loadings)) {
    config <- loadings |>
      as_tibble() |>
      select(
        observed = From,
        latent = To,
        load = Coefficient # match schema name
      ) |>
      left_join(
        config,
        by = join_by(observed, latent)
      )
  }
  # match schema name
  config <- rename(config, sil = sil_width)
  if (startsWith(name, "thresh")) {
    parsed <- str_match(
      name,
      "thresh_(?<crit>.+)_(?<level>.+)"
    )
    level <- as.numeric(parsed[, "level"]) / 100
    crit <- parsed[, "crit"]
    config <- config |>
      filter(.data[[crit]] > level)
  }
  if (startsWith(name, "top")) {
    parsed <- str_match(
      name,
      "top_(?<crit>.+)_(?<n>.+)"
    )
    n <- as.integer(parsed[, "n"])
    crit <- parsed[, "crit"]
    config <- config |>
      filter(
        row_number(desc(.data[[crit]])) <= n,
        .by = cluster
      )
  }
  if (startsWith(name, "adjusted")) {
    config <- config |>
      filter(
        sil > 0.5,
        !str_detect(
          observed,
          str_c(tasks_biased, collapse = "|")
        )
      )
  }
  select(config, observed, latent)
}

targets_cfa <- tarchetypes::tar_map(
  hypers_config_dims |>
    dplyr::mutate(
      call_config = purrr::map(
        name,
        ~ if (stringr::str_detect(.x, "load")) {
          bquote(
            prepare_config(.(.x), config_dims, loadings)
          )
        } else {
          bquote(
            prepare_config(.(.x), config_dims)
          )
        }
      )
    ),
  names = name,
  tar_target(
    config,
    call_config
  ),
  tarchetypes::tar_map(
    hypers_model,
    tar_fit_cfa(
      config,
      indices_cogstruct,
      theory = theory
    )
  )
)

list(
  tarchetypes::tar_file_read(
    indices_cogstruct,
    path_obj_from_proj("indices_cogstruct", "prepare_source_data"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    config_dims,
    # the best model is schema: thin, n_fact: 7
    path_obj_from_proj("config_thin_7", "explore_factors"),
    read = qs::qread(!!.x)
  ),
  targets_cfa,
  tar_target(
    loadings,
    parameters::model_parameters(
      fit_fo_full,
      component = "loading"
    )
  ),
  tarchetypes::tar_combine(
    gofs,
    zutils::select_list(targets_cfa, starts_with("gof")),
    command = list(!!!.x) |>
      map(\(x) as_tibble(x)) |>
      bind_rows(.id = ".id") |>
      zutils::separate_wider_dsv(
        ".id",
        c(names(hypers_model), names(hypers_config_dims)),
        patterns = c(".+?", ".+"),
        prefix = "gof"
      )
  ),
  tarchetypes::tar_combine(
    scores_factor,
    zutils::select_list(targets_cfa, starts_with("scores")),
    command = list(!!!.x) |>
      map(
        \(x) {
          unclass(x) |>
            as_tibble(rownames = "user_id") |>
            mutate(user_id = bit64::as.integer64(user_id))
        }
      ) |>
      bind_rows(.id = ".id") |>
      zutils::separate_wider_dsv(
        ".id",
        c(names(hypers_model), names(hypers_config_dims)),
        patterns = c(".+?", ".+"),
        prefix = "scores"
      )
  )
)
