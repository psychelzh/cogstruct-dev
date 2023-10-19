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
