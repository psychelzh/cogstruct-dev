#' Validate Raw Data
#'
#' This step will remove invalid data following these rules:
#' 1. Only use the data from the correct version ([check_version()]).
#' 1. Remove data of invalid device (keyboard required, but used mouse).
#'
#' @param data_parsed Data with parsed raw data.
#' @param games_req_kb Character vector contains names of the games requiring
#'   keyboard response.
#' @return Validated data of class [data.frame()].
validate_raw_parsed <- function(data_parsed, games_req_kb) {
  data_parsed |>
    check_version() |>
    filter(
      # some games require keyboard input
      map2_lgl(
        raw_parsed, game_name,
        ~ !(check_used_mouse(.x, .y) & .y %in% games_req_kb)
      )
    )
}
check_version <- function(data) {
  data |>
    mutate(
      ver_major = game_version |>
        str_extract("\\d+\\.\\d+\\.\\d+") |>
        numeric_version() |>
        _[, 1]
    ) |>
    filter(ver_major == max(ver_major)) |>
    select(-ver_major)
}
check_used_mouse <- function(raw_parsed, game_name) {
  if (!has_name(raw_parsed, "device")) {
    return(TRUE)
  }
  # keyboard press of right-arrow was recorded as "mouse" device
  if (game_name %in% c("注意警觉", "注意指向")) {
    raw_parsed$device <- if_else(
      raw_parsed$resp == "right",
      "keyboard",
      raw_parsed$device
    )
  }
  raw_parsed$device |>
    str_c(collapse = "-") |>
    str_split("-") |>
    map_lgl(~ any(.x == "mouse"))
}
