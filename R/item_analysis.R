calc_test_retest <- function(indices) {
  indices_retest <- indices |>
    inner_join(
      data.iquizoo::game_indices,
      by = join_by(game_id, index_name == index_main)
    ) |>
    filter(is.finite(score)) |>
    mutate(ver_major = str_extract(game_version, "\\d")) |>
    group_by(user_id, ver_major, index_name) |>
    filter(row_number(desc(game_time)) <= 2) |>
    mutate(is_retest = row_number(game_time) > 1) |>
    ungroup() |>
    filter(sum(is_retest) > 30, .by = c(ver_major, index_name)) |>
    mutate(
      occasion = case_match(
        row_number(game_time),
        1 ~ "test",
        2 ~ "retest"
      ),
      .by = c(user_id, ver_major, index_name)
    ) |>
    pivot_wider(
      id_cols = c(ver_major, user_id, index_name),
      names_from = occasion,
      values_from = score
    ) |>
    drop_na() |>
    select(-user_id)
  if (nrow(indices_retest) == 0 || !has_name(indices_retest, "retest")) {
    return()
  }
  bind_rows(
    raw = indices_retest,
    rm_out = indices_retest |>
      filter(
        !possibly(
          ~ performance::check_outliers(
            .x,
            method = "mcd"
          ),
          # treat all as normal if failed
          otherwise = FALSE
        )(pick(test, retest)),
        .by = c(ver_major, index_name)
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
      .by = c(ver_major, origin, index_name)
    )
}
