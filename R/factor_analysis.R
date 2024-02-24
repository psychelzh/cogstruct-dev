# exploratory factor analysis section ----
# https://www.open-access.bcu.ac.uk/6076/
iterate_efa <- function(r, n_obs = 100) { # nolint: cyclocomp_linter.
  if (!psych::isCorrelation(r)) {
    n_obs <- nrow(r)
    r <- cor(r, use = "pairwise.complete.obs")
  }
  nfact <- psych::fa.parallel(r, n_obs, plot = FALSE)$nfact
  efa <- psych::fa(r, nfact, n_obs)
  removed <- list(
    communality_too_small = character(),
    loading_too_small = character(),
    loading_cross = character()
  )
  repeat {
    stable <- TRUE
    # ensure all communality is above 0.2
    repeat {
      communality_too_small <- efa$communality < 0.2
      if (!any(communality_too_small)) {
        break
      }
      stable <- FALSE
      removed$communality_too_small <- c(
        removed$communality_too_small,
        names(which(communality_too_small))
      )
      r <- r[!communality_too_small, !communality_too_small]
      efa <- psych::fa(r, nfact, n_obs)
    }
    # ensure all factors have at least 3 loadings above 0.4
    repeat {
      loadings <- loadings(efa)
      if (sum(apply(loadings, 2, function(x) sum(x > 0.4)) < 3) == 0) {
        break
      }
      stable <- FALSE
      nfact <- nfact - 1
      efa <- psych::fa(r, nfact, n_obs)
    }
    # ensure items with no loading above 0.3 are removed
    repeat {
      loadings <- loadings(efa)
      loading_too_small <- apply(loadings, 1, max) < 0.3
      if (!any(loading_too_small)) {
        break
      }
      stable <- FALSE
      removed$loading_too_small <- c(
        removed$loading_too_small,
        names(which(loading_too_small))
      )
      r <- r[!loading_too_small, !loading_too_small]
      efa <- psych::fa(r, nfact, n_obs)
    }
    # ensure no cross-loading
    repeat {
      loadings <- loadings(efa)
      loading_cross <- apply(loadings, 1, check_cross_loading)
      if (!any(loading_cross)) {
        break
      }
      stable <- FALSE
      removed$loading_cross <- c(
        removed$loading_cross,
        names(which(loading_cross))
      )
      r <- r[!loading_cross, !loading_cross]
      efa <- psych::fa(r, nfact, n_obs)
    }
    if (stable) {
      break
    }
  }
  list(
    n_fact = nfact,
    efa = efa,
    vars = colnames(r),
    removed = removed
  )
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
fit_efa_g <- function(data, vars, ...) {
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

# helper functions ----
check_cross_loading <- function(x) {
  # also check negative cross-loading
  x <- abs(x)
  x_cross <- x[x > 0.3]
  if (length(x_cross) < 2) {
    return(FALSE)
  }
  if (all(x_cross < 0.4) || min(x_cross) > 0.75 * max(x_cross)) {
    return(TRUE)
  }
  return(FALSE)
}
