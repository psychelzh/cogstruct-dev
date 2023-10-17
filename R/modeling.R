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

fit_cfa <- function(model, data, ...) {
  lavaan::cfa(
    model,
    data,
    std.ov = TRUE,
    std.lv = TRUE,
    missing = "ml",
    ...
  )
}

predict_dim <- function(fitted, data, suffix = "") {
  lavaan::lavPredict(fitted) |>
    unclass() |>
    as_tibble() |>
    add_column(user_id = data$user_id, .before = 1L) |>
    pivot_longer(
      -user_id,
      names_to = "dim_simple",
      values_to = paste0("score_dim", suffix)
    )
}

efa_to_cfa <- function(fit,
                       hierarchical = c("none", "bifactor", "highorder")) {
  extract_efa_params(fit) |>
    prepare_model(
      col_dim = "mr",
      col_task = "game_index",
      hierarchical = hierarchical
    )
}

resample_fact_attribution <- function(data, n_fact, exclude = character()) {
  data |>
    select(-contains(exclude)) |>
    slice_sample(prop = 1, replace = TRUE) |>
    psych::fa(n_fact) |>
    extract_efa_params(drop_load = TRUE) |>
    chop(game_index)
}

extract_efa_params <- function(fit,
                               name_task = "game_index",
                               name_mr = "mr",
                               name_load = "load",
                               drop_load = TRUE) {
  params <- parameters::model_parameters(fit, threshold = "max") |>
    rename(game_index = Variable) |>
    select(-Complexity, -Uniqueness) |>
    pivot_longer(
      starts_with("MR"),
      names_to = name_mr,
      values_to = name_load,
      values_drop_na = TRUE
    )
  if (drop_load) params[[name_load]] <- NULL
  params
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
