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

# names mappings ----
match_game_index <- function(game_id, index_name = NULL) {
  game_name_abbr <- data.iquizoo::match_info(game_id, "game_name_abbr")
  if (is.null(index_name)) {
    index_name <- data.iquizoo::game_indices |>
      filter(game_id %in% {{ game_id }}) |>
      pull(index_main)
    if (length(index_name) != length(game_id)) {
      stop(
        "Some game does not match single index name. ",
        "Please specify `index_name` explicitly."
      )
    }
  }
  str_c(game_name_abbr, index_name, sep = ".")
}

replace_as_name_cn <- function(game_index,
                               remove_suffix = FALSE,
                               delim = ".") {
  splitted <- str_split(game_index, fixed(delim), simplify = TRUE)
  splitted[, 1] <- splitted[, 1] |>
    data.iquizoo::match_info(to = "game_name", from = "game_name_abbr")
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

# functional programming ----
lapply_tar_batches <- function(.l, ..., .append = FALSE) {
  out <- lapply(zutils::select_list(.l, !starts_with("tar")), ...)
  if (.append) {
    out <- c(out, zutils::select_list(.l, starts_with("tar")))
  }
  out
}

list_rbind_tar_batches <- function(l, names_to = rlang::zap(), append = FALSE) {
  main <- zutils::select_list(l, !starts_with("tar"))
  # remove names if names are all empty strings
  if (all(names(main) == "")) names(main) <- NULL
  out <- list_rbind(main, names_to = names_to)
  if (append) {
    out <- add_column(out, !!!zutils::select_list(l, starts_with("tar")))
  }
  out
}

# cpm related ----
bind_rows_meta <- function(..., .names, .prefix) {
  patterns <- rep(".+?", length(.names))
  # should be greedy because there are "_" in `config` field
  patterns[.names == "config"] <- ".+"
  bind_rows(..., .id = ".id") |>
    zutils::separate_wider_dsv(
      ".id",
      .names,
      patterns = patterns,
      prefix = .prefix
    )
}
