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
    arrange(game_time) |>
    distinct(pick({{ id_cols }}), game_name, index_name, .keep_all = TRUE) |>
    left_join(data.iquizoo::game_info, by = c("game_id", "game_name")) |>
    select({{ id_cols }}, game_id, game_name, game_name_abbr, game_time,
           index_name, score)
}
