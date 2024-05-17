censor_indices <- function(indices, ...,
                           users_completed, res_motivated,
                           id_cols_extra = NULL) {
  indices |>
    # remove users who did not complete the experiment
    semi_join(users_completed, by = "user_id") |>
    # Keep the latest data for each user.
    filter(
      row_number(desc(game_time)) == 1,
      .by = c(user_id, game_id, index_name, {{ id_cols_extra }})
    ) |>
    bind_rows(...) |>
    inner_join(game_indices, by = join_by(game_id, index_name)) |>
    mutate(
      score_adj = if_else(inverse, -score, score),
      game_index = match_game_index(game_id, index_name)
    ) |>
    left_join(
      res_motivated |>
        filter(row_number(desc(game_time)) == 1, .by = c(user_id, game_id)) |>
        select(user_id, game_id, is_motivated),
      by = c("user_id", "game_id")
    ) |>
    mutate(
      is_outlier_iqr = score %in% boxplot.stats(score)$out,
      .by = c(game_id, index_name, {{ id_cols_extra }})
    )
}

reshape_indices <- function(indices, users_clean, col_values = score_adj) {
  indices |>
    # fulfill screening before reshaping
    semi_join(filter(users_clean, keep), by = "user_id") |>
    filter(!is_outlier_iqr & is_motivated) |>
    pivot_wider(
      id_cols = user_id,
      names_from = game_index,
      values_from = {{ col_values }}
    ) |>
    column_to_rownames("user_id")
}

screen_users <- function(indices) {
  n_indices <- n_distinct(indices$game_index)
  indices |>
    summarise(
      prop_invalid = sum(is_outlier_iqr | !is_motivated) / n_indices,
      .by = user_id
    ) |>
    mutate(keep = prop_invalid <= thresh_prop_invalid)
}

prepare_users_demography <- function(users, indices) {
  users |>
    left_join(
      indices |>
        summarise(
          game_date = median(game_time),
          .by = user_id
        ),
      by = "user_id"
    ) |>
    mutate(
      user_age = (user_dob %--% game_date) / years(),
      .keep = "unused",
      .after = user_sex
    )
}

split_data_solomon <- function(data, n = 2) {
  pca_result <- psych::principal(
    data,
    nfactors = ncol(data),
    rotate = "none",
    missing = TRUE
  )
  groups_source <- rep(
    as.vector(t(arrangements::permutations(n))),
    length.out = nrow(data)
  )
  groups <- with(
    pca_result,
    groups_source[rank(scores %*% colMeans(loadings^2))]
  )
  split(data, groups)
}
