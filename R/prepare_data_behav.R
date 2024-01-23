clean_indices <- function(indices, users_completed, res_motivated, id_cols) {
  indices |>
    semi_join(users_completed, by = "user_id") |>
    # keep the last result for each user and game index
    # https://github.com/r-lib/vctrs/issues/1787
    arrange(desc(game_time)) |>
    distinct(pick(all_of(id_cols)), game_id, index_name, .keep_all = TRUE) |>
    left_join(
      res_motivated,
      by = setdiff(colnames(res_motivated), "is_motivated")
    ) |>
    mutate(
      is_outlier_iqr = score %in% boxplot.stats(score)$out,
      .by = c(game_id, index_name)
    )
}

reshape_indices <- function(indices, id_cols, col_score = "score_adj") {
  indices |>
    filter(
      !is_outlier_iqr & is_motivated,
      # RAPM test is not included in the factor analysis
      game_id != game_id_rapm
    ) |>
    pivot_wider(
      id_cols = all_of(id_cols),
      names_from = game_index,
      values_from = {{ col_score }}
    ) |>
    # remove participants with more than 20% missing data
    filter(rowMeans(is.na(pick(!all_of(id_cols)))) <= 0.2)
}
