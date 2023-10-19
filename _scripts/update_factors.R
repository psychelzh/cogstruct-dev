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

store_preproc <- fs::path(
  tar_config_get("store", project = "preproc_behav"),
  "objects"
)

thresh_by_cutoff <- function(params, level) {
  cutoff_loadings <- c(poor = 0.32, fair = 0.45, good = 0.55)
  params |>
    filter(Coefficient >= cutoff_loadings[level]) |>
    filter(n() >= 3, .by = To)
}
thresh_by_numtask <- function(params, level) {
  cutoff_numtasks <- c(three = 3, four = 4)
  params |>
    slice_max(
      Coefficient,
      n = cutoff_numtasks[level],
      by = To
    )
}
thresh_by_trimdim <- function(params, level) {
  params |>
    filter(To != level)
}
add_targets_fitting <- function(dims, col_dim, col_task, suffix = NULL) {
  add_suffix <- function(name) {
    if (!is.null(suffix)) {
      name <- paste0(name, "_", suffix)
    }
    name
  }
  list(
    tar_target_raw(
      add_suffix("model"),
      rlang::expr(
        prepare_model(
          !!rlang::enexpr(dims),
          hierarchical = hierarchical,
          col_dim = !!col_dim,
          col_task = !!col_task
        )
      )
    ),
    tar_target_raw(
      add_suffix("fit"),
      rlang::expr(
        fit_cfa(
          !!rlang::sym(add_suffix("model")),
          indices_wider_clean,
          orthogonal = hierarchical == "bifactor"
        )
      )
    ),
    tar_target_raw(
      add_suffix("gof"),
      rlang::expr(
        fitmeasures(!!rlang::sym(add_suffix("fit")))
      )
    )
  )
}

hypers_model <- list(hierarchical = c("none", "bifactor", "highorder"))
hypers_updation <- dplyr::bind_rows(
  data.frame(
    method = "cutoff",
    level = c("poor", "fair", "good")
  ),
  data.frame(
    method = "numtask",
    level = c("three", "four")
  ),
  data.frame(
    method = "trimdim",
    level = readr::read_csv(
      "config/dimensions.csv",
      show_col_types = FALSE
    ) |>
      dplyr::pull(dim_abbr) |>
      unique()
  )
) |>
  dplyr::mutate(thresh_fun = rlang::syms(paste0("thresh_by_", method)))
targets_origin <- tarchetypes::tar_map(
  values = hypers_model,
  add_targets_fitting(
    dims_origin |>
      filter(dim_abbr != "Uns"),
    suffix = "origin",
    col_dim = "dim_abbr",
    col_task = "game_index"
  )
)
targets_updation <- tarchetypes::tar_map(
  values = hypers_updation,
  names = -thresh_fun,
  list(
    tar_target(
      dims_updated,
      thresh_fun(model_params_ref, level)
    ),
    tarchetypes::tar_map(
      values = hypers_model,
      add_targets_fitting(
        dims_updated,
        suffix = "updated",
        col_dim = "To",
        col_task = "From"
      )
    )
  )
)
targets_updation2 <- tarchetypes::tar_map(
  values = hypers_updation |>
    dplyr::filter(method == "trimdim"),
  names = -thresh_fun,
  list(
    tar_target(
      dims_updated2,
      thresh_fun(model_params_updated, level)
    ),
    tarchetypes::tar_map(
      values = hypers_model,
      add_targets_fitting(
        dims_updated2,
        suffix = "updated2",
        col_dim = "To",
        col_task = "From"
      )
    )
  )
)
targets_gof <- lapply(
  hypers_model$hierarchical,
  \(hier_type) {
    name <- paste0("gof_updated_", hier_type)
    combine_targets(
      name,
      targets_updation,
      cols_targets = c("method", "level"),
      fun = \(x) as_tibble_row(unclass(x))
    )
  }
)
targets_gof2 <- lapply(
  hypers_model$hierarchical,
  \(hier_type) {
    name <- paste0("gof_updated2_", hier_type)
    combine_targets(
      name,
      targets_updation2,
      cols_targets = c("method", "level"),
      fun = \(x) as_tibble_row(unclass(x))
    )
  }
)

list(
  tarchetypes::tar_file_read(
    indices_wider_clean,
    fs::path(store_preproc, "indices_wider_clean"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_file_read(
    dims_origin,
    "config/dimensions.csv",
    read = read_csv(!!.x, show_col_types = FALSE)
  ),
  targets_origin,
  combine_targets(
    gof_origin,
    targets_origin,
    cols_targets = "hierarchical",
    fun = \(x) as_tibble_row(unclass(x))
  ),
  tar_target(
    model_params_ref,
    fit_origin_none |>
      parameters::model_parameters() |>
      filter(Component == "Loading") |>
      as_tibble()
  ),
  targets_updation,
  targets_gof,
  combine_targets(
    dims_updated,
    targets_updation,
    cols_targets = c("method", "level")
  ),
  tarchetypes::tar_combine(
    gof_updated,
    targets_gof,
    command = bind_rows(!!!.x, .id = "hierarchical") |>
      mutate(hierarchical = str_remove(hierarchical, "gof_updated_"))
  ),
  tar_target(
    model_params_updated,
    fit_updated_none_numtask_three |>
      parameters::model_parameters() |>
      filter(Component == "Loading") |>
      as_tibble()
  ),
  targets_updation2,
  targets_gof2,
  tarchetypes::tar_combine(
    gof_updated2,
    targets_gof2,
    command = bind_rows(!!!.x, .id = "hierarchical") |>
      mutate(hierarchical = str_remove(hierarchical, "gof_updated2_"))
  )
)
