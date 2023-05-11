clean_indices <- function(indices, users_completed) {
  indices |>
    semi_join(users_completed, by = "user_id") |>
    left_join(data.iquizoo::game_info, by = c("game_id", "game_name")) |>
    mutate(
      game_name_real = if_else(
        str_detect(game_name_abbr, "[A|B]$"),
        str_remove(game_name, "[A|B]$"),
        game_name
      ),
      game_name_abbr = str_remove(game_name_abbr, "[A|B]$")
    ) |>
    select(user_id, game_id, game_name, game_name_abbr, game_time,
           index_name, score)
}
