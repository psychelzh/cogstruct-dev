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

prepare_config <- function(config, name) {
  switch(name,
    full = config,
    bigsil = config |>
      filter(
        row_number(desc(sil_width)) <= 5 |
          sil_width > mean(sil_width),
        .by = cluster
      ),
    if (startsWith(name, "top")) {
      n <- as.integer(str_remove(name, "top"))
      config |>
        filter(
          row_number(desc(sil_width)) <= n,
          .by = cluster
        )
    }
  )
}

targets_cfa <- tarchetypes::tar_map(
  hypers_config_dims,
  tar_target(
    config,
    prepare_config(config_dims, name)
  ),
  tarchetypes::tar_map(
    hypers_model_type,
    tar_fit_cfa(
      indices_wider_clean,
      config,
      col_latent = dim_label,
      col_manifest = game_index,
      hierarchical = hierarchical
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
    path_obj_from_proj("cluster_result", "explore_factors"),
    read = qs::qread(!!.x) |>
      filter(schema == "thin") |>
      pluck("silinfo", 1)
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
  tarchetypes::tar_combine(
    results,
    zutils::select_list(targets_cfa, starts_with("results")),
    command = zutils::vec_rbind_meta(
      !!!.x,
      .names_meta = c(
        names(hypers_model_type),
        names(hypers_config_dims)
      )
    )
  )
)
