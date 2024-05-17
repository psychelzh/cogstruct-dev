extract_simple <- function(file) {
  load_coefs(file) |>
    mutate(
      index_name = str_extract(index, "^.*(?=_)"),
      .before = score,
      .keep = "unused"
    )
}

extract_switch <- function(file) {
  coefs <- load_coefs(file)
  names_cond <- c("repeat", "switch")
  index_prefix <- "switch_cost"
  extract_diff(coefs, names_cond, index_prefix)
}

extract_cong <- function(file) {
  coefs <- load_coefs(file)
  names_cond <- c("con", "inc")
  index_prefix <- "cong_eff"
  extract_diff(coefs, names_cond, index_prefix)
}

extract_alert <- function(file) {
  coefs <- load_coefs(file)
  names_cond <- c(audio = "aud", visual = "vis", none = "neu")
  index_prefix <- "alert"
  extract_diff2(coefs, names_cond, index_prefix)
}

extract_orient <- function(file) {
  coefs <- load_coefs(file)
  names_cond <- c(endogenous = "endo", exogenous = "exo", neutral = "neu")
  index_prefix <- "orient"
  extract_diff2(coefs, names_cond, index_prefix)
}

# helper functions ----
load_coefs <- function(file) {
  coef(readRDS(file))[[1]] |>
    apply(3, tibble::as_tibble, rownames = "user_id") |>
    list_rbind(names_to = "index") |>
    mutate(
      game_id = bit64::as.integer64(
        str_extract(file, "(?<=game-)[[:alnum:]]+")
      ),
      user_id = bit64::as.integer64(user_id),
      index = ifelse(
        grepl("^(bs|ndt)", index),
        index,
        paste0("v_", index)
      ),
      occasion = if (str_detect(file, "retest")) {
        str_extract(index, "test|retest")
      },
      score = Estimate,
      .keep = "none"
    )
}

extract_diff <- function(coefs, names_cond, index_prefix) {
  coefs |>
    mutate(
      par = str_extract(index, "^.*(?=_)"),
      cond = str_extract(index, paste(names_cond, collapse = "|")),
      index_name = if_else(
        is.na(cond),
        par,
        paste(cond, par, sep = "_")
      )
    ) |>
    pivot_wider(
      id_cols = any_of(c("game_id", "user_id", "occasion")),
      values_from = score,
      names_from = index_name
    ) |>
    mutate(
      "{index_prefix}_v" := .data[[str_c(names_cond[[1]], "_v")]] -
        .data[[str_c(names_cond[[2]], "_v")]]
    ) |>
    pivot_longer(
      !any_of(c("game_id", "user_id", "occasion")),
      names_to = "index_name",
      values_to = "score"
    )
}

extract_diff2 <- function(coefs, names_cond, index_prefix) {
  coefs |>
    mutate(
      par = str_extract(index, "^.*(?=_)"),
      cond = names_cond[
        str_extract(index, paste(names(names_cond), collapse = "|"))
      ],
      .keep = "unused"
    ) |>
    pivot_wider(values_from = score, names_from = cond) |>
    mutate(
      "{index_prefix}_{names_cond[[1]]}" :=
        .data[[names_cond[[1]]]] - .data[[names_cond[[3]]]],
      "{index_prefix}_{names_cond[[2]]}" :=
        .data[[names_cond[[2]]]] - .data[[names_cond[[3]]]],
      "{index_prefix}" :=
        (.data[[names_cond[[1]]]] + .data[[names_cond[[2]]]]) / 2 -
        .data[[names_cond[[3]]]]
    ) |>
    pivot_wider(
      names_from = par,
      values_from = !any_of(c("game_id", "user_id", "occasion", "par"))
    ) |>
    pivot_longer(
      !any_of(c("game_id", "user_id", "occasion")),
      names_to = "index_name",
      values_to = "score"
    )
}
