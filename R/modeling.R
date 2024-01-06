fit_cfa <- function(config, data, ...,
                    col_latent = latent,
                    col_manifest = manifest,
                    hierarchical = c("none", "bifactor", "highorder")) {
  rlang::check_dots_used()
  hierarchical <- match.arg(hierarchical)
  prepare_model(config, {{ col_latent }}, {{ col_manifest }}, hierarchical) |>
    cfa(
      data,
      std.ov = TRUE,
      std.lv = TRUE,
      missing = "ml",
      orthogonal = hierarchical == "bifactor",
      ...
    )
}

prepare_model <- function(config, col_latent, col_manifest,
                          hierarchical = c("none", "bifactor", "highorder")) {
  hierarchical <- match.arg(hierarchical)
  str_c_safe <- function(x, collapse = NULL) {
    str_c("`", x, "`", collapse = collapse)
  }
  model <- config |>
    summarise(
      manifests = str_c_safe({{ col_manifest }}, collapse = " + "),
      .by = {{ col_latent }}
    ) |>
    summarise(
      spec = str_c(
        {{ col_latent }}, " =~ ", manifests,
        collapse = "\n"
      )
    )
  if (hierarchical != "none") {
    g_from <- switch(hierarchical,
      bifactor = unique(pull(config, {{ col_manifest }})),
      highorder = unique(pull(config, {{ col_latent }}))
    )
    model$spec <- model$spec |>
      str_c(
        str_c("g =~ ", str_c_safe(g_from, collapse = " + ")),
        sep = "\n"
      )
  }
  model$spec
}

extract_latent_scores <- function(fit, data = NULL, id_cols_data = NULL) {
  scores <- as_tibble(unclass(lavPredict(fit, data)))
  if (!is.null(data)) {
    id_cols_data <- substitute(id_cols_data) %||% 1
    scores <- bind_cols(select(data, {{ id_cols_data }}), scores)
  }
  scores
}
