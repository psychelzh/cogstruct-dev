calc_reliability <- function(indices) {
  indices_retest <- indices |>
    inner_join(
      data.iquizoo::game_indices,
      by = join_by(game_id, index_name == index_main)
    ) |>
    filter(is.finite(score)) |>
    group_by(user_id, index_name) |>
    filter(row_number(desc(game_time)) <= 2) |>
    mutate(
      occasion = case_match(
        row_number(game_time),
        1 ~ "test",
        2 ~ "retest"
      )
    ) |>
    ungroup() |>
    pivot_wider(
      id_cols = c(game_id, user_id, index_name),
      names_from = occasion,
      values_from = score
    ) |>
    drop_na() |>
    select(-user_id)
  if (!has_name(indices_retest, "retest") || nrow(indices_retest) < 30) {
    return()
  }
  bind_rows(
    raw = indices_retest,
    rm_out = indices_retest |>
      filter(
        !performance::check_outliers(
          pick(c(test, retest)),
          method = "mcd"
        )
      ),
    .id = "origin"
  ) |>
    reframe(
      tibble(
        n = n(),
        r = cor(test, retest),
        # use ICC(2, 1)
        icc = psych::ICC(pick(test, retest))$results$ICC[[2]]
      ),
      .by = c(game_id, origin, index_name)
    )
}
