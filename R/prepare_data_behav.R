censor_indices <- function(indices, subset, users_completed, res_motivated,
                           id_cols_extra = NULL) {
  indices |>
    filter({{ subset }}) |>
    # remove users who did not complete the experiment
    semi_join(users_completed, by = "user_id") |>
    # Keep the latest data for each user.
    filter(
      row_number(desc(game_time)) == 1,
      .by = c(user_id, game_id, index_name, {{ id_cols_extra }})
    ) |>
    data.iquizoo::screen_indices() |>
    mutate(
      game_index = game_id |>
        data.iquizoo::match_info(
          from = "game_id",
          to = "game_name_abbr"
        ) |>
        str_c(index_name, sep = ".")
    ) |>
    left_join(
      res_motivated,
      by = intersect(names(indices), names(res_motivated))
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
  groups <- with(pca_result, rank(scores %*% colMeans(loadings^2)) %% n)
  split(data, groups)
}
