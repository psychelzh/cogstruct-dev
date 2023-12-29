# check missing trials ----
check_prop_miss <- function(data, name_acc, name_rt) {
  if (has_name(data, name_acc)) {
    return(mean(data[[name_acc]] == -1) <= thresh_prop_miss)
  } else if (has_name(data, name_rt)) {
    return(mean(data[[name_rt]] == 0) <= thresh_prop_miss)
  } else {
    stop("No info of missing.")
  }
}

check_prop_miss_go <- function(data, name_acc, name_rt) {
  check_prop_miss(
    filter(data, type == "go"),
    name_acc = name_acc,
    name_rt = name_rt
  )
}

# check fast guess trials ----
check_fast_guess <- function(data, name_rt, thresh_rt) {
  rts <- data[[name_rt]]
  if (is.character(rts)) {
    rts <- as.numeric(unlist(str_split(rts, "\\D")))
  }
  # fast guess trials should be few enough (do not check missing here)
  mean(rts > 0 & rts < thresh_rt) <= thresh_prop_guess
}

check_fast_guess_items <- function(data, name_rt, thresh_rt) {
  # these tasks are timed, so we check the first half items only
  data |>
    slice_head(prop = 0.5) |>
    check_fast_guess(
      name_rt = name_rt,
      thresh_rt = thresh_rt
    )
}

check_fast_guess_rapm <- function(data, name_rt, thresh_rt) {
  data |>
    filter(block == 2) |>
    check_fast_guess_items(
      name_rt = name_rt,
      thresh_rt = thresh_rt
    )
}

check_fast_guess_average <- function(data, name_rt, name_acc, thresh_rt) {
  data |>
    mutate(
      rt = .data[[name_rt]] /
        lengths(str_split(.data[[name_acc]], "\\D"))
    ) |>
    check_fast_guess(
      name_rt = "rt",
      thresh_rt = thresh_rt
    )
}

# check levels for adaptive ----
check_adaptive <- function(data, name_level) {
  mean(data[[name_level]] <= data[[name_level]][[1]]) < 0.5
}

# check error proportions ----
check_pe <- function(data, chance, name_acc) {
  binom.test(
    sum(data[[name_acc]] == 1),
    nrow(data),
    p = chance,
    alternative = "greater"
  )$p.value < 0.05
}

check_pe_nback <- function(data, chance, name_acc) {
  check_pe(
    filter(data, type != "filler"),
    chance = chance,
    name_acc = name_acc
  )
}

check_pe_go <- function(data, chance, name_acc) {
  check_pe(
    filter(data, type == "go"),
    chance = chance,
    name_acc = name_acc
  )
}

check_pe_simple <- function(data, name_level, chance, name_acc) {
  check_pe(
    filter(data, .data[[name_level]] == min(.data[[name_level]])),
    chance = chance,
    name_acc = name_acc
  )
}

check_ne <- function(data) {
  !any(data$falsenum > data$correctnum)
}

# check correct proportions ----
check_pc <- function(data, thresh_pc, name_acc) {
  acc <- data[[name_acc]]
  if (is.character(acc)) {
    acc <- unlist(str_split(acc, "\\D"))
  }
  mean(acc == 1, na.rm = TRUE) >= thresh_pc
}

check_pc_simple <- function(data, thresh_pc, name_acc, name_level) {
  check_pc(
    filter(data, .data[[name_level]] <= .data[[name_level]][[1]]),
    thresh_pc = thresh_pc,
    name_acc = name_acc
  )
}

check_pc_simple_dist <- function(data, thresh_pc, name_acc, name_level) {
  data |>
    mutate(
      acc = str_split(.data[[name_acc]], "-") |>
        map_chr(~ str_c(as.integer(as.numeric(.x) <= 1), collapse = "-"))
    ) |>
    check_pc_simple(
      thresh_pc = thresh_pc,
      name_acc = "acc",
      name_level = name_level
    )
}

check_pc_supp <- function(data, thresh_pc, name_acc_supp) {
  check_pc(
    data,
    thresh_pc = thresh_pc,
    name_acc = name_acc_supp
  )
}

check_pc_stop <- function(data) {
  between(
    mean(data$acc[data$type != "go"] == 1),
    0.25, 0.75
  )
}

# special case for tower of London ----
check_min_move <- function(data) {
  !any(data$stepsused < 4)
}

# special case for SynWin ----
check_synwin <- function(data) {
  is_valid_cake <- data |>
    filter(status %in% c("flip", "drag")) |>
    check_pc(thresh_pc = 0.25, name_acc = "acc")
  is_valid_soup <- data |>
    filter(status %in% 0:10) |>
    check_pc(thresh_pc = 0.5, name_acc = "acc")
  is_valid_oven <- data |>
    filter(status %in% c("low", "high")) |>
    check_pe(chance = 0.5, name_acc = "acc")
  is_valid_cake & is_valid_soup & is_valid_oven
}

# special case for JLO ----
check_jlo <- function(data) {
  mean(
    preproc.iquizoo:::calc_angle_err(
      data$resp, data$angle,
      resp_anti = "left",
      resp_clock = "right"
    ) > 6
  ) < 0.25
}
