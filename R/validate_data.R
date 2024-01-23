#' Validate Raw Data
#'
#' @description
#' The validation process contains the following steps:
#'
#' 1. Keep data with the largest major version only: e.g., remove 2.9.0 and
#' keep 3.0.0 or 3.1.0.
#' 1. Check if keyboard is used for certain tasks which require keyboard
#' responses.
#' 1. Check if data names were valid, for some error data contains data from
#' another game caused by the technical issues.
#' 1. Keep the latest data for each user.
#'
#' @param data_parsed Data with parsed raw data.
#' @param require_keyboard Logical indicating if keyboard response is required.
#' @param list_names A [list()] of possible column names.
#' @return Validated data of class [data.frame()].
#' @export
validate_data <- function(data_parsed, require_keyboard, list_names) {
  data_parsed |>
    filter(check_ver(game_version)) |>
    filter(map_lgl(raw_parsed, check_device, require_keyboard)) |>
    filter(map_lgl(raw_parsed, check_names, list_names)) |>
    filter(row_number(desc(game_time)) == 1, .by = user_id)
}

# helper functions
check_ver <- function(version) {
  ver_major <- str_extract(version, "\\d+")
  ver_major == max(ver_major)
}

check_device <- function(raw_parsed, require_keyboard) {
  if (!require_keyboard) {
    return(TRUE)
  }
  !"mouse" %in% unlist(str_split(raw_parsed$device, "-"))
}

check_names <- function(raw_parsed, list_names) {
  if (is.null(list_names)) {
    return(TRUE)
  }
  list(colnames(raw_parsed)) %in% list_names
}
