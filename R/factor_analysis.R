# exploratory factor analysis section ----
resample_fact_attribution <- function(data, n_fact, exclude = character()) {
  data |>
    select(!contains(exclude)) |>
    slice_sample(prop = 1, replace = TRUE) |>
    psych::fa(n_fact) |>
    parameters::model_parameters(threshold = "max") |>
    pivot_longer(
      starts_with("MR"),
      names_to = "mr",
      values_to = "loading",
      values_drop_na = TRUE
    ) |>
    select(mr, game_index = Variable) |>
    chop(game_index)
}

extract_prob_one_fact <- function(fact_attribution) {
  fact_attribution |>
    mutate(
      pairs = map(
        game_index,
        ~ expand.grid(x = .x, y = .x)
      ),
      .keep = "unused"
    ) |>
    unnest(pairs) |>
    xtabs(~ x + y, data = _)
}

output_factcons <- function(schema, mat, ...,
                            file_prefix = "factcons",
                            dir_output = "_output/factor-consistency") {
  file <- fs::path(
    dir_output,
    str_glue("{file_prefix}_schema-{schema}.png")
  )
  rownames(mat) <- replace_as_name_cn(rownames(mat))
  colnames(mat) <- replace_as_name_cn(colnames(mat))
  ragg::agg_png(file, width = 1980, height = 1980, res = 100)
  corrplot::corrplot(
    mat,
    type = "upper",
    method = "color",
    order = "hclust",
    hclust.method = "ward.D2",
    is.corr = FALSE
  )
  dev.off()
  file
}

# Confirmatory factor analysis section ----

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
