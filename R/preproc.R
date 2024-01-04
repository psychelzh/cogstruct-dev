#' Validate Raw Data
#'
#' @param data_parsed Data with parsed raw data.
#' @param require_keyboard Logical indicating if keyboard response is required.
#' @param list_names A [list()] of possible column names.
#' @return Validated data of class [data.frame()].
validate_data <- function(data_parsed, require_keyboard, list_names) {
  #' The validation process contains the following steps:
  #'
  #' 1. Keep data with the largest major version only: e.g., remove 2.9.0 and
  #' keep 3.0.0 or 3.1.0.
  ver_major <- str_extract(data_parsed$game_version, "\\d+")
  ver_keep <- ver_major == max(ver_major)

  #' 1. Check if keyboard is used for certain tasks which require keyboard
  #' responses.
  dev_keep <- check_device(data_parsed, require_keyboard)

  #' 1. Check if data names were valid, for some error data contains data from
  #' another game caused by the technical issues.
  names_keep <- TRUE
  if (!is.null(list_names)) {
    names_keep <- map_lgl(
      data_parsed$raw_parsed,
      ~ list(colnames(.x)) %in% list_names
    )
  }

  data_parsed[ver_keep & dev_keep & names_keep, ]
}

# helper functions
check_device <- function(data_parsed, require_keyboard) {
  if (!require_keyboard) {
    return(TRUE)
  }
  map_lgl(
    data_parsed$raw_parsed,
    ~ !"mouse" %in% unlist(str_split(.x$device, "-"))
  )
}
