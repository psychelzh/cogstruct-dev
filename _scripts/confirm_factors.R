library(targets)
tar_source()
tar_option_set(
  package = c("tidyverse", "bit64", "lavaan"),
  format = "qs",
  memory = "transient",
  garbage_collection = TRUE,
  controller = if (Sys.info()["nodename"] == "shadow") {
    crew.cluster::crew_controller_sge(
      name = "cfa",
      workers = 40,
      seconds_idle = 30
    )
  } else {
    crew::crew_controller_local(
      name = "cfa-local",
      workers = 16,
      seconds_idle = 10
    )
  }
)

prepare_config <- function(name, config, loadings = NULL) {
  if (!is.null(loadings)) {
    config <- loadings |>
      as_tibble() |>
      select(
        dim_label = To,
        game_index = From,
        load = Coefficient # match schema name
      ) |>
      left_join(
        config,
        by = c("dim_label", "game_index")
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
          game_index,
          str_c(tasks_biased, collapse = "|")
        )
      )
  }
  select(config, dim_label, game_index)
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
      indices_wider_clean,
      col_manifest = game_index,
      col_latent = dim_label,
      theory = theory
    )
  )
)

list(
  tarchetypes::tar_file_read(
    indices_wider_clean,
    path_obj_from_proj("indices_wider_clean", "prepare_source_data"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    dim_silinfo,
    # the best model is schema: thin, n_fact: 7
    path_obj_from_proj("config_thin_7", "explore_factors"),
    read = qs::qread(!!.x)
  ),
  tar_target(
    file_dim_labels,
    "config/dimensions.csv",
    format = "file"
  ),
  tar_target(
    config_dims,
    left_join(
      dim_silinfo,
      read_csv(file_dim_labels, show_col_types = FALSE),
      by = "cluster"
    )
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
    command = bind_rows(!!!.x, .id = ".id") |>
      zutils::separate_wider_dsv(
        ".id",
        c(names(hypers_model), names(hypers_config_dims)),
        patterns = c(".+?", ".+"),
        prefix = "scores"
      )
  )
)
