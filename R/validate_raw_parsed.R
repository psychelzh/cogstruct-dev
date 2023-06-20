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
    ) |>
    # correct VR data
    mutate(
      raw_parsed = ifelse(
        game_name == "文字推理",
        map(
          raw_parsed,
          ~ mutate(
            .x,
            acc = map2_int(
              cresp, resp,
              ~ all(
                str_split(.x, ",") %in%
                  str_split(.y, ",")
              )
            )
          )
        ),
        raw_parsed
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

check_motivated <- function(raw_parsed, game_name, chance_acc,
                            rigor = TRUE, ...) {
  if (game_name == "变色魔块PRO") {
    raw_parsed <- filter(raw_parsed, type == "go")
  }
  if (game_name %in% c("图片记忆A", "强化学习")) {
    raw_parsed <- filter(raw_parsed, phase == "test")
  }
  if (has_name(raw_parsed, "type")) {
    raw_parsed <- filter(raw_parsed, type != "filler")
  }
  if (game_name %in% c("方向检测", "色彩检测")) {
    raw_parsed <- filter(raw_parsed, numtarget %in% c(3, 5))
  }
  if (game_name == "塔罗牌") {
    raw_parsed <- slice_tail(raw_parsed, prop = 0.5)
  }
  # more 10% missed trials
  if (has_name(raw_parsed, "acc") && mean(raw_parsed$acc == -1) > 0.1) {
    return(FALSE)
  }
  if (!is.na(chance_acc)) {
    if (rigor) {
      return(
        sum(raw_parsed$acc == 1) >
          qbinom(0.95, nrow(raw_parsed), chance_acc)
      )
    } else {
      return(mean(raw_parsed$acc == 1) > chance_acc)
    }
  }
  return(TRUE)
  # if (method == "rt") {
  #   if (has_name(raw_parsed, "acc")) {
  #     raw_parsed <- raw_parsed |>
  #       filter(acc != -1)
  #   }
  #   return(mean(raw_parsed$rt < chance * 1000) < 0.1)
  # }
}
