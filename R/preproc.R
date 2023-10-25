#' Validate Raw Data
#'
#' This step will remove invalid data following these rules:
#' 1. Only use the data from the correct version ([check_version()]).
#' 1. Remove data of invalid device (keyboard required, but used mouse).
#'
#' @param data_parsed Data with parsed raw data.
#' @param require_keyboard Logical indicating if keyboard response is required.
#' @return Validated data of class [data.frame()].
validate_data <- function(data_parsed, require_keyboard) {
  game_id <- data_parsed$game_id[[1]]

  # keep data with the largest major version only
  # e.g., remove 2.9.0 and keep 3.0.0 or 3.1.0
  ver_major <- str_extract(data_parsed$game_version, "\\d+")
  ver_keep <- ver_major == max(ver_major)

  dev_keep <- check_device(data_parsed, require_keyboard)

  data_parsed[ver_keep & dev_keep, ]
}

#' Cleanse the calculated scores
#'
#' The most important job here is to replace the original scores by the
#' corresponding makeup scores.
clean_indices <- function(indices, users_completed,
                          id_cols = user_id) {
  indices |>
    semi_join(users_completed, by = "user_id") |>
    # keep the first result for each subject and game
    # https://github.com/r-lib/vctrs/issues/1787
    arrange(desc(game_time)) |>
    distinct(pick({{ id_cols }}), game_id, index_name, .keep_all = TRUE) |>
    left_join(data.iquizoo::game_info, by = c("game_id")) |>
    select({{ id_cols }}, game_id, game_name, game_name_abbr,
           game_time, game_duration, index_name, score)
}

clean_indices_short <- function(indices, contents) {
  contents |>
    inner_join(indices, by = join_by(project_id, game_id)) |>
    inner_join(data.iquizoo::game_info, by = join_by(game_id)) |>
    inner_join(
      data.iquizoo::game_indices,
      by = join_by(game_id, index_name == index_main)
    ) |>
    arrange(game_time) |>
    distinct(user_id, game_id, course_period, .keep_all = TRUE) |>
    mutate(score = if_else(index_reverse, -score, score))
}

# The separate scores for face and vocation cannot be calculated
fname_slices <- function(data, .by = NULL, .input = NULL, .extra = NULL) {
  data |>
    summarise(fntotal = sum(acc == 1), .by = all_of(.by))
}

# helper functions ----
check_device <- function(data_parsed, require_keyboard) {
  if (!require_keyboard) {
    return(TRUE)
  }
  if (data_parsed$game_id[[1]] %in%
      c("380173315257221", "380174783693701")) {
    # "注意警觉", "注意指向"
    data_parsed <- correct_device_issue(data_parsed)
  }
  map_lgl(data_parsed$raw_parsed, is_valid_device)
}

correct_device_issue <- function(data_parsed) {
  data_parsed |>
    mutate(
      raw_parsed = map2(
        raw_parsed, game_version,
        \(raw_parsed, game_version) {
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
      )
    )
}

is_valid_device <- function(raw_parsed) {
  !"mouse" %in% unlist(str_split(raw_parsed$device, "-"))
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
