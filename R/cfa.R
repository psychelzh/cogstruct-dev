#' Fit a confirmatory factor analysis model
#'
#' This is just a wrapper of [lavaan::cfa()] that allows the model to be
#' specified in a data frame and allows users to choose from four pre-defined
#' theories of the model.
#'
#' Specifically, `"of"` is one-factor model, `"fo"` is a first-order model,
#' `"ho"` is a higher-order model, and `"bf"` is a bi-factor model. See Brunner
#' et al. (2012) for detailed discussion of the models and the naming
#' conventions used here.
#'
#' @param config A data frame with columns `latent` and `manifest` that
#'   specifies the model. The `latent` column specifies the latent variables and
#'   the `manifest` column specifies the manifest variables. The `latent` column
#'   can be omitted if the model is one-factor model.
#' @param data A data frame with the manifest variables.
#' @param ... Other arguments passed to `cfa()`.
#' @param col_manifest,col_latent The name of the column in `config` that
#'   specifies the manifest and latent variables.
#' @param theory The theory of the model. See details.
#' @return The same as [lavaan::cfa()].
#' @export
fit_cfa <- function(config, data, ...,
                    col_manifest = manifest,
                    col_latent = latent,
                    theory = c("of", "fo", "ho", "bf")) {
  rlang::check_dots_used()
  theory <- match.arg(theory)
  prepare_model(config, {{ col_manifest }}, {{ col_latent }}, theory) |>
    cfa(
      data,
      std.ov = TRUE,
      std.lv = TRUE,
      missing = "ml",
      orthogonal = theory == "bf",
      ...
    )
}

prepare_model <- function(config, col_manifest, col_latent, theory) {
  str_c_safe <- function(x, collapse = NULL) {
    str_c("`", x, "`", collapse = collapse)
  }
  model_g <- switch(theory,
    of = ,
    bf = str_c(
      "g =~ ",
      str_c_safe(
        unique(pull(config, {{ col_manifest }})),
        collapse = " + "
      )
    ),
    ho = str_c(
      "g =~ ",
      str_c_safe(
        unique(pull(config, {{ col_latent }})),
        collapse = " + "
      )
    )
  )
  model_facts <- if (theory != "of") {
    config |>
      summarise(
        manifests = str_c_safe({{ col_manifest }}, collapse = " + "),
        .by = {{ col_latent }}
      ) |>
      summarise(
        spec = str_c(
          {{ col_latent }}, " =~ ", manifests,
          collapse = "\n"
        )
      ) |>
      pull(spec)
  }
  str_c(model_g, model_facts, sep = "\n")
}

extract_latent_scores <- function(fit, data = NULL, id_cols_data = NULL) {
  scores <- as_tibble(unclass(lavPredict(fit, data)))
  if (!is.null(data)) {
    id_cols_data <- substitute(id_cols_data) %||% 1
    scores <- bind_cols(select(data, {{ id_cols_data }}), scores)
  }
  scores
}
