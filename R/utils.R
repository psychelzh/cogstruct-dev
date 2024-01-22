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

# misc ----
replace_as_name_cn <- function(game_index,
                               remove_suffix = FALSE,
                               delim = ".") {
  splitted <- str_split(game_index, fixed(delim), simplify = TRUE)
  splitted[, 1] <- splitted[, 1] |>
    data.iquizoo::match_info(from = "game_name_abbr", to = "game_name")
  if (remove_suffix) splitted[, 1] <- str_remove(splitted[, 1], "[a-zA-Z]+$")
  str_c(splitted[, 1], splitted[, 2], sep = delim)
}

match_cases <- function(data, subjs) {
  data_subjs <- attr(data, "id")
  matched <- match(subjs, data_subjs)
  if (anyNA(matched)) {
    stop("Some subjects are not found in the data.")
  }
  structure(
    data[matched, ],
    id = attr(data, "id")[matched]
  )
}

match_info <- function(x, from = "game_name_abbr", to = "game_id") {
  if (!requireNamespace("bit64", quietly = TRUE)) {
    stop("`bit64` package must be installed to continue.")
  }
  data.iquizoo::game_info[[to]][match(x, data.iquizoo::game_info[[from]])]
}
