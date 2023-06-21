#' Fit a one-g factor model for given observed variables
#'
#' @param data Raw behavior data.
#' @param vars A character vector specifying observed variables.
#' @returns A fitted one-g factor model.
#' @export
#' @import lavaan
fit_g <- function(data, vars) {
  efa(data, ov.names = vars, std.ov = TRUE, missing = "ml")
}

#' Calculate the variance explained by g factor
#'
#' @param fit A fitted one-g factor model.
#' @returns A numeric value indicating the variance explained by g factor.
#' @export
calc_var_exp <- function(fit) {
  mean(loadings(fit)^2)
}

#' Predict g factor scores
#'
#' @param data Raw behavior data.
#' @param mdl A fitted one-g factor model.
#' @param id_cols A numeric vector specifying the column indices of subject
#'   identifiers.
#' @returns A data frame with g factor scores.
#' @export
predict_g_score <- function(data, mdl, id_cols = 1, name_g = "g") {
  g <- lavPredict(mdl)[, 1]
  data_names <- rownames(loadings(mdl))
  for (data_name in data_names) {
    test <- cor.test(g, data[[data_name]], use = "pairwise")
    # if g is anti-correlated significantly with any ov, inverse it
    if (test$estimate < 0 && test$p.value < 0.05) {
      g <- -g
      break
    }
  }
  add_column(data[, id_cols], "{name_g}" := g)
}

# 2-fold CVs ----
split_data <- function(indices_pool) {
  distinct(indices_pool, user_id) |>
    mutate(set = c("train", "test")[sample(row_number() %% 2 + 1)]) |>
    left_join(indices_pool, by = "user_id")
}

feature_selection <- function(data, game_durs, dimensions, scores_dim, n_each = 3) {
  data |>
    left_join(game_durs, by = c("game_name", "part")) |>
    left_join(dimensions, by = "game_index") |>
    inner_join(scores_dim, by = c("user_id", "cfa")) |>
    summarise(
      n = sum(!is.na(score_adj)),
      var_exp_total = cor(score_adj, score_dim)^2,
      .by = c(label, cfa, game_index, part, mean_dur_mins)
    ) |>
    filter(n > 200) |>
    mutate(
      var_exp_per_min = var_exp_total / mean_dur_mins,
      var_exp_crit = case_when(
        cfa %in% c("Inh", "Shift") ~ var_exp_total,
        label == "EM" & part < 1 ~ 0,
        .default = var_exp_per_min
      )
    ) |>
    arrange(desc(var_exp_crit)) |>
    distinct(cfa, game_index, .keep_all = TRUE) |>
    slice_max(
      order_by = var_exp_crit,
      n = n_each,
      by = cfa
    )
}

evaluate_selection <- function(data, features, scores_dim) {
  model <- features |>
    summarise(
      formula_str = str_c(game_index, collapse = " + "),
      .by = cfa
    ) |>
    summarise(
      spec = str_c(cfa, formula_str, sep = " =~ ", collapse = "\n")
    ) |>
    pull(spec)
  model_data <- data |>
    semi_join(features, by = c("game_index", "part")) |>
    pivot_wider(
      id_cols = user_id,
      names_from = game_index,
      values_from = score_adj
    )
  fit_dim(model, model_data) |>
    predict_dim(model_data, suffix = "_part") |>
    left_join(
      scores_dim,
      by = c("user_id", "cfa")
    ) |>
    summarise(
      var_exp = cor(score_dim_part, score_dim)^2,
      .by = cfa
    )
}

evaluate_selection_g <- function(data, features, scores_g) {
  model_data <- data |>
    semi_join(features, by = c("game_index", "part")) |>
    pivot_wider(
      id_cols = user_id,
      names_from = game_index,
      values_from = score_adj
    )
  fit_g(model_data, features$game_index) |>
    predict_g_score(model_data, mdl = _, name_g = "g_part") |>
    left_join(scores_g, by = "user_id") |>
    summarise(var_exp = cor(g, g_part)^2)
}

fit_dim <- function(model, data) {
  lavaan::cfa(
    model, data,
    std.ov = TRUE, std.lv = TRUE, missing = "ml"
  )
}

predict_dim <- function(fitted, data, suffix = "") {
  lavaan::lavPredict(fitted) |>
    unclass() |>
    as_tibble() |>
    add_column(user_id = data$user_id, .before = 1L) |>
    pivot_longer(
      -user_id,
      names_to = "cfa",
      values_to = paste0("score_dim", suffix)
    )
}
