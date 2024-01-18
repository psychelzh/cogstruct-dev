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
  map_names <- pull(data.iquizoo::game_info, game_name, name = game_name_abbr)
  splitted[, 1] <- map_names[splitted[, 1]]
  if (remove_suffix) splitted[, 1] <- str_remove(splitted[, 1], "[a-zA-Z]+$")
  str_c(splitted[, 1], splitted[, 2], sep = delim)
}
