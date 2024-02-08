library(targets)
tar_source()
tar_option_set(
  packages = c("tidyverse", "bit64", "lavaan"),
  format = "qs",
  controller = setup_crew_controller("efa")
)
setup_parallel_plan()

list(
  tarchetypes::tar_file_read(
    indices_cogstruct,
    path_obj_from_proj("indices_cogstruct", "prepare_source_data"),
    read = qs::qread(!!.x)
  ),
  tar_target(
    indices_splitted,
    split_data_solomon(select(indices_cogstruct, !contains(games_thin)))
  ),
  tar_target(
    efa_result,
    iterate_efa(indices_splitted[[1]])
  ),
  tar_target(
    config_dims,
    efa_result$efa |>
      parameters::model_parameters(threshold = "max") |>
      pivot_longer(
        starts_with("MR"),
        names_to = "latent",
        values_to = "loading",
        values_drop_na = TRUE
      ) |>
      select(latent, manifest = Variable)
  ),
  tarchetypes::tar_map(
    hypers_model,
    tar_fit_cfa(
      config_dims,
      indices_splitted[[2]],
      theory = theory
    )
  )
)
