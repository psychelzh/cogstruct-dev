fit_cfa <- function(model, data, ...) {
  cfa(
    model,
    data,
    std.ov = TRUE,
    std.lv = TRUE,
    missing = "ml",
    ...
  )
}

prepare_model <- function(config,
                          col_dim = "dim",
                          col_task = "game_name",
                          hierarchical = c("none", "bifactor", "highorder")) {
  hierarchical <- match.arg(hierarchical)
  model <- config |>
    summarise(
      rhs = str_c("`", .data[[col_task]], "`", collapse = " + "),
      .by = all_of(col_dim)
    ) |>
    summarise(
      spec = str_c(.data[[col_dim]], " =~ ", rhs, collapse = "\n")
    ) |>
    pull(spec)
  if (hierarchical != "none") {
    g_level <- switch(hierarchical,
      bifactor = col_task,
      highorder = col_dim
    )
    model <- str_c(
      model,
      str_c(
        "g =~ ",
        str_c("`", unique(config[[g_level]]), "`", collapse = " + ")
      ),
      sep = "\n"
    )
  }
  model
}
