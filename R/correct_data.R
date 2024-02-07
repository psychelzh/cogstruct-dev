#' Correct some specific issues in the data
#'
#' @name correct_data
NULL

#' @rdname correct_data
correct_device <- function(data) {
  data |>
    mutate(
      raw_parsed = map2(
        raw_parsed, game_version,
        correct_device_issue
      )
    )
}

#' @rdname correct_data
correct_game_dur <- function(data) {
  data |>
    mutate(game_duration = game_duration / 1000)
}

#' @rdname correct_data
correct_mst <- function(data) {
  data |>
    mutate(
      raw_parsed = lapply(
        raw_parsed,
        \(raw_parsed) filter(raw_parsed, phase == "test")
      )
    )
}

#' @rdname correct_data
correct_rt <- function(data, adjust) {
  data |>
    mutate(
      raw_parsed = lapply(
        raw_parsed,
        \(raw_parsed) mutate(raw_parsed, rt = rt + adjust)
      )
    )
}

# helper functions
correct_device_issue <- function(raw_parsed, game_version) {
  raw_parsed |>
    mutate(
      device = if_else(
        # 1.0.0 erroneously records device for right resp as "mouse"
        resp == "right" & game_version == "1.0.0",
        "keyboard",
        device
      )
    )
}
