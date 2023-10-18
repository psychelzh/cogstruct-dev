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
  values = dplyr::bind_rows(
    data.frame(
      method = "cutoff",
      level = c("poor", "fair", "good")
    ),
    data.frame(
      method = "numtask",
      level = c("three", "four")
    )
  ) |>
    dplyr::mutate(thresh_fun = rlang::syms(paste0("thresh_by_", method))),
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
targets_gof <- lapply(
  hypers_model$hierarchical,
  \(hier_type) {
    name <- paste0("gof_updated_", hier_type)
    tarchetypes::tar_combine_raw(
      name,
      targets_updation[[name]],
      command = list(!!!.x) |>
        lapply(\(x) as_tibble_row(unclass(x))) |>
        bind_rows(.id = "id") |>
        separate_wider_delim(
          id,
          delim = "_",
          names = c(NA, NA, "hierarchical", "method", "level")
        ) |>
        substitute()
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
  tar_target(
    model_params_ref,
    fit_origin_none |>
      parameters::model_parameters() |>
      filter(Component == "Loading") |>
      as_tibble()
  ),
  targets_updation,
  tarchetypes::tar_combine(
    dims_updated,
    targets_updation$dims_updated,
    command = bind_rows(!!!.x, .id = "id") |>
      separate_wider_delim(
        id,
        delim = "_",
        names = c(NA, NA, "method", "level")
      )
  ),
  targets_gof,
  tarchetypes::tar_combine(gof_updated, targets_gof)
)
