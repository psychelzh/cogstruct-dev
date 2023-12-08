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
  tar_path_obj_from_proj(
    file_crit,
    "scores_origin_bifactor",
    project = "confirm_factors"
  ),
  tar_path_obj_from_proj(
    file_users_completed,
    "users_completed",
    project = "prepare_source_data"
  ),
  tarchetypes::tar_map(
    contents,
    names = game_id,
    tar_path_obj_from_proj(
      file_data,
      sprintf("data_valid_%s", game_id),
      project = "prepare_source_data"
    )
  ),
  targets_indices_partitioned,
  tarchetypes::tar_combine(
    indices_slices,
    targets_indices_partitioned
  ),
  tar_target(
    indices_slices_clean,
    clean_indices(
      indices_slices,
      qs::qread(file_users_completed),
      id_cols = c(user_id, part)
    )
  ),
  tar_target(
    indices_slices_of_interest,
    indices_slices_clean |>
      inner_join(
        data.iquizoo::game_indices,
        join_by(game_id, index_name == index_main)
      ) |>
      mutate(score_adj = if_else(index_reverse, -score, score)) |>
      mutate(
        is_outlier_iqr = score %in% boxplot.stats(score)$out,
        .by = c(game_id, index_name, part)
      )
  )
)
