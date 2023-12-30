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

replace_as_name_cn <- function(name, remove_suffix = FALSE) {
  parts <- str_split(name, "\\.")
  out <- character(length = length(name))
  for (i in seq_along(name)) {
    this_parts <- parts[[i]]
    this_parts[[1]] <- data.iquizoo::game_info |>
      select(game_name_abbr, game_name) |>
      deframe() |>
      _[this_parts[[1]]]
    if (remove_suffix) {
      this_parts[[1]] <- str_remove(
        this_parts[[1]],
        "[a-zA-Z]+$"
      )
    }
    out[[i]] <- str_c(this_parts, collapse = ".")
  }
  out
}

output_factcons <- function(schema, mat, ...,
                            file_prefix = "factcons",
                            dir_output = "_output/factor-consistency") {
  file <- fs::path(
    dir_output,
    str_glue("{file_prefix}_schema-{schema}.png")
  )
  rownames(mat) <- replace_as_name_cn(rownames(mat))
  colnames(mat) <- replace_as_name_cn(colnames(mat))
  ragg::agg_png(file, width = 1980, height = 1980, res = 100)
  corrplot::corrplot(
    mat,
    type = "upper",
    method = "color",
    order = "hclust",
    hclust.method = "ward.D2",
    col.lim = c(0, 1),
    col = corrplot::COL2("RdBu")
  )
  dev.off()
  file
}

read_archived <- function(...) {
  select(
    targets::tar_read(...),
    !contains("name")
  )
}

select_list <- function(.l, ...) {
  pos <- tidyselect::eval_select(rlang::expr(c(...)), .l)
  rlang::set_names(.l[pos], names(pos))
}

retract_tbl_to_mat <- function(.data, sort_names = TRUE) {
  stopifnot(ncol(.data) == 3)
  attr <- colnames(.data)[[3]]
  mat <- igraph::graph_from_data_frame(.data, directed = FALSE) |>
    igraph::as_adj(attr = attr) |>
    as.matrix()
  if (sort_names) {
    order <- sort(colnames(mat))
    mat <- mat[order, order]
  }
  mat
}

call_full <- function(.fn) {
  rlang::call2(.fn, !!!syms_args(.fn))
}

syms_args <- function(.fn) {
  args <- formalArgs(rlang::as_function(.fn))
  setNames(rlang::syms(args), args)
}
