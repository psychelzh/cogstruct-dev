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

targets_indices_partitioned <- tar_partition_rawdata(
  tarflow.iquizoo::fetch_iquizoo_mem()(
    readr::read_file("sql/contents_camp.sql")
  ),
  project_rawdata = "prepare_source_data"
)

list(
  tar_target(
    file_users_completed,
    path_obj_from_proj("users_completed", "prepare_source_data"),
    format = "file"
  ),
  tarchetypes::tar_file_read(
    durations,
    path_obj_from_proj("durations", "prepare_source_data"),
    read = qs::qread(!!.x)
  ),
  targets_indices_partitioned,
  tarchetypes::tar_combine(
    indices_slices,
    select_list(targets_indices_partitioned, contains("indices_slices"))
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
