clean_retest <- function(indices, extra_by = NULL) {
  indices_clean <- indices |>
    filter(is.finite(score)) |>
    mutate(ver_major = str_extract(game_version, "\\d")) |>
    group_by(user_id, ver_major, index_name, pick(all_of(extra_by))) |>
    filter(row_number(desc(game_time)) <= 2) |>
    filter(n() == 2) |>
    mutate(occasion = c("test", "retest")[row_number(game_time)]) |>
    ungroup()
  if (nrow(indices_clean) == 0) {
    return()
  }
  users_clean <- indices_clean |>
    distinct(
      project_id, user_id, ver_major, index_name, game_time, occasion,
      pick(all_of(extra_by))
    ) |>
    pivot_wider(
      id_cols = c(ver_major, user_id, index_name, all_of(extra_by)),
      names_from = occasion,
      # keep all useful information
      values_from = c(project_id, game_time)
    ) |>
    mutate(days_retest = (game_time_test %--% game_time_retest) / days()) |>
    # interval should be 5-14 days
    filter(between(round(days_retest), 5, 14))
  if (nrow(users_clean) <= 30) {
    return()
  }
  indices_clean |>
    pivot_wider(
      id_cols = c(ver_major, user_id, index_name, all_of(extra_by)),
      names_from = occasion,
      values_from = score
    ) |>
    inner_join(
      users_clean,
      by = c("ver_major", "user_id", "index_name", extra_by)
    )
}

calc_test_retest <- function(data, ..., col_test = test, col_retest = retest) {
  bind_rows(
    raw = data,
    rm_out = data |>
      filter(
        !possibly(
          performance::check_outliers,
          otherwise = FALSE # treat all as normal if failed
        )(data, method = "mcd")
      ),
    .id = "origin"
  ) |>
    summarise(
      n = n(),
      r = cor({{ col_test }}, {{ col_retest }}),
      # use ICC(2, 1)
      icc = psych::ICC(pick({{ col_test }}, {{ col_retest }}))$results$ICC[[2]],
      .by = "origin"
    )
}
