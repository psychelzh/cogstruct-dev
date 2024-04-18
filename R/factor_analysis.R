# exploratory factor analysis section ----
# for bootstrap factor analysis
prepare_procrustes_target <- function(x, col_ov = "rhs", col_lv = "lhs") {
  x["id"] <- seq_len(nrow(x))
  keys_list <- map(split(x, x[[col_lv]]), "id")
  psych::make.keys(nrow(x), keys_list, x[[col_ov]])
}

get_procrusted_loadings <- function(data, target) {
  EFA.dimensions::PROCRUSTES(
    psych::fa(data[rownames(target)], ncol(target), rotate = "none")$loadings,
    target,
    type = "oblique",
    verbose = FALSE
  )
}

trim_loadings <- function(loadings_bootstrap) {
  # mimic 0.4-0.3-0.2 rule as 0.3-0.2-0.2 rule:
  # 0.3 for primary, 0.2 for alternative, 0.2 for cross-loading difference
  loadings <- apply(loadings_bootstrap, 1:2, mean)
  loadings_pri <- Rfast::rowMaxs(abs(loadings), value = TRUE)
  loadings_alt <- Rfast::rownth(
    abs(loadings), rep(2, nrow(loadings)),
    descending = TRUE
  )
  # low loading: primary < 0.3
  is_low <- loadings_pri < 0.3
  # cross-loading cond 1: alternative loadings larger than 0.2
  is_cross_1 <- loadings_alt > 0.2
  # cross-loading cond 2: difference between primary and alternative < 0.2
  is_cross_2 <- loadings_pri - loadings_alt < 0.2
  loadings[!is_low & !is_cross_1 & !is_cross_2, ]
}

# Confirmatory factor analysis section ----

#' Fit a confirmatory factor analysis model
#'
#' This is just a wrapper of [lavaan::cfa()] that allows the model to be
#' specified in a data frame and allows users to choose from four pre-defined
#' theories of the model.
#'
#' @param config A data frame with columns `latent` and `manifest` that
#'   specifies the model. The `latent` column specifies the latent variables and
#'   the `manifest` column specifies the manifest variables. The `latent` column
#'   can be omitted if the model is one-factor model.
#' @param data A data frame with the manifest variables.
#' @param theory The theory of the model. Should be one of `"fo"` (first-order),
#'   `"ho"` (higher-order), `"bf"` (bi-factor) and `"of"` (one-factor). See
#'   Brunner et al. (2012) for detailed discussion of the models and the naming
#'   conventions used here.
#' @param col_ov,col_lv The name of the column in `config` that specifies the
#'   manifest and latent variables.
#' @param col_fix The name of the column in `config` that specifies the fixed
#'   parameters. Only used for loadings. If `NULL`, all parameters are free.
#' @param missing The method for handling missing data. See
#'   [lavaan::lavOptions()] for details.
#' @return The same as [lavaan::cfa()].
#' @references
#'
#' Brunner, M., Nagy, G., & Wilhelm, O. (2012). A Tutorial on Hierarchically
#' Structured Constructs. Journal of Personality, 80(4), 796–846.
#' https://doi.org/10.1111/j.1467-6494.2011.00749.x
#' @export
fit_cfa <- function(config, data, theory,
                    col_ov = manifest,
                    col_lv = latent,
                    col_fix = NULL,
                    missing = "ml") {
  theory <- match.arg(theory, c("fo", "ho", "bf", "of"))
  model <- prepare_model(
    config,
    theory,
    {{ col_ov }},
    {{ col_lv }},
    {{ col_fix }}
  )
  zutils::cautiously(cfa)(
    model,
    data,
    std.ov = TRUE,
    std.lv = TRUE,
    missing = missing,
    orthogonal = theory == "bf"
  )
}

prepare_model <- function(config, theory, col_ov, col_lv,
                          col_fix = NULL) {
  # ensure supporting quosure
  no_fix <- rlang::quo_is_null(rlang::enquo(col_fix))
  config <- config |>
    rename(
      manifest = {{ col_ov }},
      latent = {{ col_lv }}
    )
  if (!no_fix) {
    config <- config |>
      rename(fix = {{ col_fix }})
  }
  # no g factor in first-order theory
  model_g <- if (theory != "fo") {
    str_glue_data(
      config,
      switch(theory,
        of = ,
        bf = "g =~ {manifest}",
        ho = "g =~ {latent}"
      )
    ) |>
      unique()
  }
  # no group factors in one-factor theory
  model_facts <- if (theory != "of") {
    str_glue_data(
      config,
      if (no_fix) {
        "{latent} =~ {manifest}"
      } else {
        "{latent} =~ {fix} * {manifest}"
      }
    )
  }
  c(model_g, model_facts)
}

extract_latent_scores <- function(fit, data = NULL) {
  scores <- lavPredict(fit, data)
  if (!is.null(data)) {
    # lavaan do not keep the rownames of the data
    rownames(scores) <- rownames(data)
  }
  scores
}

# Special for g factor estimation ----
fit_efa_g <- function(data, vars = names(data), ...) {
  efa(
    data,
    ov.names = vars,
    std.ov = TRUE,
    std.lv = TRUE,
    ...
  )
}

extract_g_scores <- function(fit, data) {
  scores <- extract_latent_scores(fit, data)
  # inverse g if anti-correlated with the largest loading variable
  loadings <- loadings(fit)
  name_max_loading <- rownames(loadings)[which.max(abs(loadings))]
  if (cor(scores, data[[name_max_loading]], use = "pairwise") < 0) {
    scores <- -scores
  }
  scores
}
