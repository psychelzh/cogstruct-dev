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
correct_cr <- function(data, correction) {
  data |>
    mutate(
      raw_parsed = lapply(
        raw_parsed,
        \(raw_parsed) correct_cr_acc_issue(raw_parsed, correction)
      )
    )
}

#' @rdname correct_data
correct_vr <- function(data) {
  data |>
    mutate(raw_parsed = lapply(raw_parsed, correct_vr_acc_issue))
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

correct_cr_acc_issue <- function(raw_parsed, correction) {
  raw_parsed |>
    select(-acc) |>
    left_join(correction, by = c("concept", "word"))
}

correct_vr_acc_issue <- function(raw_parsed) {
  raw_parsed |>
    mutate(
      acc = map2_int(
        cresp, resp,
        ~ setequal(
          str_split_1(.x, ","),
          str_split_1(.y, ",")
        )
      )
    )
}
