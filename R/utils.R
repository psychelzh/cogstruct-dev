# files processing ----
path_obj_from_proj <- function(object, project) {
  fs::path(
    targets::tar_config_get(
      "store",
      project = project
    ),
    "objects",
    object
  )
}

read_archived <- function(...) {
  tryCatch(
    select(
      targets::tar_read(...),
      !contains("name")
    ),
    error = function(e) {
      warning(conditionMessage(e))
      invisible()
    }
  )
}

# misc ----
replace_as_name_cn <- function(game_index,
                               remove_suffix = FALSE,
                               delim = ".") {
  tibble(game_index) |>
    separate_wider_delim(
      game_index, delim,
      names = c("game_name_abbr", "index_name")
    ) |>
    left_join(
      data.iquizoo::game_info |>
        select(game_name_abbr, game_name),
      by = join_by(game_name_abbr)
    ) |>
    mutate(
      game_name = if (remove_suffix) {
        str_remove(game_name, "[a-zA-Z]+$")
      } else {
        game_name
      }
    ) |>
    unite(res, game_name, index_name, sep = delim) |>
    pull(res)
}

retract_tbl_to_mat <- function(.data, ..., sort_names = TRUE) {
  rlang::check_dots_used()
  stopifnot(ncol(.data) == 3)
  attr <- colnames(.data)[[3]]
  mat <- igraph::graph_from_data_frame(.data, ...) |>
    igraph::as_adj(attr = attr) |>
    as.matrix()
  if (sort_names) {
    order <- sort(colnames(mat))
    mat <- mat[order, order]
  }
  mat
}
