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
predict_g_score <- function(data, mdl, id_cols = 1) {
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
  add_column(data[, id_cols], g = g)
}

#' Regress out covariates
#'
#' @param data A data frame with subject identifiers and outcome variable.
#' @param subjs_info A data frame with subject identifiers and covariates.
#' @param covars A character vector specifying covariates.
#' @returns A data frame with residuals.
#' @export
regress_covariates <- function(data, subjs_info, covars) {
  name_outcome <- names(data)[2]
  formula <- as.formula(
    paste(name_outcome, "~", paste(covars, collapse = " + "))
  )
  data_combined <- left_join(data, subjs_info, by = "sub_id")
  data[[2]] <- resid(lm(formula, data = data_combined, na.action = na.exclude))
  data
}
