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

match_dim_label <- function(latent) {
  dimensions <- read_csv("config/dimensions.csv", show_col_types = FALSE) |>
    mutate(latent = str_c("F", cluster)) |>
    add_row(latent = "g", dim_label = "g") |>
    pull(dim_label, name = latent)
  dimensions[latent]
}

lapply_tar_batches <- function(.l, ..., .append = FALSE) {
  out <- .l |>
    zutils::select_list(!starts_with("tar")) |>
    lapply(...)
  if (.append) {
    out <- c(out, zutils::select_list(.l, starts_with("tar")) )
  }
  out
}
