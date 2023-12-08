library(targets)
future::plan(future.callr::callr)
tar_source()
tar_option_set(
  package = c("tidyverse", "bit64", "lavaan", "preproc.iquizoo"),
  format = "qs",
  imports = "preproc.iquizoo",
  memory = "transient",
  garbage_collection = TRUE,
  controller = crew::crew_controller_local(workers = 8)
)

projects <- targets::tar_config_yaml()
contents <- tarflow.iquizoo:::fetch_iquizoo_mem(
  readr::read_file("sql/contents_camp.sql")
) |>
  dplyr::distinct(game_id) |>
  dplyr::left_join(data.iquizoo::game_info, by = "game_id") |>
  data.iquizoo::match_preproc(type = "inner") |>
  dplyr::inner_join(
    readr::read_csv("config/game_format.csv", show_col_types = FALSE),
    by = "game_name"
  ) |>
  dplyr::filter(!is.na(format)) |>
  dplyr::mutate(
    game_id = as.character(game_id),
    prep_fun = dplyr::if_else(
      game_name == "社交达人",
      rlang::syms("fname_slices"),
      prep_fun
    ),
    tar_file_data = rlang::syms(
      stringr::str_glue("file_data_{game_id}")
    )
  )
targets_indices_partitioned <- c(
  tarchetypes::tar_map(
    contents |>
      dplyr::filter(format != "items") |>
      dplyr::mutate(
        slice_fun = rlang::syms(
          stringr::str_glue("slice_data_{format}")
        )
      ),
    names = game_id,
    tar_target(
      indices_slices,
      qs::qread(tar_file_data) |>
        slice_fun(parts, subset = subset) |>
        preproc_data(prep_fun, .input = input, .extra = extra)
    )
  ),
  tarchetypes::tar_map(
    contents |>
      dplyr::filter(format == "items"),
    names = game_id,
    tar_target(
      indices_slices,
      qs::qread(tar_file_data) |>
        slice_data_items(qs::qread(file_crit)) |>
        preproc_data(prep_fun, .input = input, .extra = extra)
    )
  )
)

list(
  tar_target(
    file_crit,
    fs::path(
      projects$confirm_factors$store,
      "objects",
      "scores_origin_bifactor"
    )
  ),
  tarchetypes::tar_map(
    contents,
    names = game_id,
    tar_target(
      file_data,
      fs::path(
        projects$prepare_source_data$store,
        "objects",
        sprintf("data_valid_%s", game_id)
      )
    )
  ),
  targets_indices_partitioned,
  tarchetypes::tar_combine(
    indices_slices,
    targets_indices_partitioned
  )
)
