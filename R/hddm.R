extract_hddm_coefs <- function(file, context = c("retest", "camp")) {
  context <- match.arg(context)
  game_id <- bit64::as.integer64(str_extract(file, "(?<=game-)[0-9]+"))
  effect <- str_extract(file, "(?<=effect-)[a-z]+")
  df_coefs <- coef(readRDS(file))[[1]] |>
    apply(3, tibble::as_tibble, rownames = "user_id") |>
    list_rbind(names_to = "index") |>
    mutate(
      game_id,
      user_id = bit64::as.integer64(user_id),
      index = ifelse(
        grepl("^(bs|ndt)", index),
        index,
        paste0("v_", index)
      )
    )
  id_cols <- c("game_id", "user_id", "occasion")
  indices <- if (effect != "simple") {
    switch(effect,
      cong = {
        names_cond <- c("con", "inc")
        index_prefix <- "cong_eff"
      },
      switch = {
        names_cond <- c("repeat", "switch")
        index_prefix <- "switch_cost"
      },
      stop("Invalid effect.")
    )
    df_coefs |>
      mutate(
        occasion = if (context == "retest") {
          str_extract(index, "test|retest")
        },
        par = str_extract(index, "^.*(?=_)"),
        cond = str_extract(index, paste(names_cond, collapse = "|")),
        index_name = if_else(
          is.na(cond),
          par,
          paste(cond, par, sep = "_")
        )
      ) |>
      pivot_wider(
        id_cols = any_of(id_cols),
        values_from = Estimate,
        names_from = index_name
      ) |>
      mutate(
        "{index_prefix}_v" := .data[[str_c(names_cond[[1]], "_v")]] -
          .data[[str_c(names_cond[[2]], "_v")]]
      ) |>
      pivot_longer(
        !any_of(id_cols),
        names_to = "index_name",
        values_to = "score"
      )
  } else {
    df_coefs |>
      mutate(
        occasion = if (context == "retest") {
          str_extract(index, "test|retest")
        },
        par = str_extract(index, "^.*(?=_)")
      ) |>
      select(any_of(c(id_cols, index_name = "par", score = "Estimate")))
  }
  if (context == "retest") {
    return(
      indices |>
        pivot_wider(
          id_cols = c(game_id, user_id, index_name),
          values_from = score,
          names_from = occasion
        )
    )
  } else {
    return(indices)
  }
}
