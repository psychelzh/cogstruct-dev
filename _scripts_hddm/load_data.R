load_data <- function(context, game_id, effect,
                      key = NULL,
                      rt_min = NA,
                      rt_max = NA) {
  rt_min <- coalesce(rt_min, 0)
  rt_max <- coalesce(rt_max, Inf)
  projects <- targets::tar_config_yaml()
  dat <- switch(context,
    retest = targets::tar_read_raw(
      paste0("data_valid_", game_id),
      store = projects$prepare_source_data_retest$store
    ) |>
      filter(n() == 2, .by = user_id) |>
      mutate(
        ocassion = if_else(row_number(game_time) == 1, "test", "retest"),
        .by = user_id
      ),
    camp = targets::tar_read_raw(
      paste0("data_valid_", game_id),
      store = projects$prepare_source_data$store
    ) |>
      filter(row_number(desc(game_time)) == 1, .by = user_id)
  )
  if (game_id == "238239294447813") {
    dat <- dat |>
      filter(map_lgl(raw_parsed, \(x) all(x[[key]] < 200)))
  }
  dat |>
    unnest(raw_parsed) |>
    mutate(
      rt = RT / 1000,
      # unify levels
      type = if (effect == "switch") {
        factor(tolower(.data[[key]]), c("repeat", "switch"))
      } else if (effect == "cong") {
        factor(str_sub(tolower(.data[[key]]), end = 3), c("con", "inc"))
      } else if (effect %in% c("orient", "alert", "nback")) {
        factor(tolower(.data[[key]]))
      } else if (effect == "anti") {
        factor(.data[[key]])
      },
      stimtype = if (effect == "comp") {
        factor(str_sub(tolower(.data$StimType), end = 3), c("con", "inc"))
      },
      tasktype = if (effect == "comp") {
        factor(tolower(.data$TaskType), c("repeat", "switch"))
      }
    ) |>
    filter(
      if_all(contains("type", ignore.case = FALSE), \(x) !is.na(x)),
      rt > rt_min,
      rt < rt_max
    ) |>
    select(c(
      "user_id",
      if (context == "retest") "ocassion",
      if (effect != "simple") contains("type", ignore.case = FALSE),
      acc = "ACC",
      "rt"
    ))
}
