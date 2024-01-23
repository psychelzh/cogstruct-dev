censor_indices <- function(indices, users_completed, res_motivated, id_cols) {
  indices |>
    # remove users who did not complete the experiment
    semi_join(users_completed, by = "user_id") |>
    # RAPM test is not included in the structure exploration
    filter(game_id != game_id_rapm) |>
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
      by = setdiff(colnames(res_motivated), "is_motivated")
    ) |>
    mutate(
      is_outlier_iqr = score %in% boxplot.stats(score)$out,
      .by = c(game_id, index_name)
    ) |>
    # add percent missing data
    mutate(
      count_invalid = sum(is_outlier_iqr | !is_motivated),
      .by = user_id
    ) |>
    mutate(percent_invalid = count_invalid / n_distinct(game_index))
}

reshape_indices <- function(indices, id_cols) {
  indices |>
    # fulfill screening before reshaping
    filter(!is_outlier_iqr & is_motivated & percent_invalid <= 0.2) |>
    pivot_wider(
      id_cols = all_of(id_cols),
      names_from = game_index,
      values_from = score_adj
    )
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
