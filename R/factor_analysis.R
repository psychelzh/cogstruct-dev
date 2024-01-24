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
#' @param config A data frame with columns `latent` and `manifest` that
#'   specifies the model. The `latent` column specifies the latent variables and
#'   the `manifest` column specifies the manifest variables. The `latent` column
#'   can be omitted if the model is one-factor model.
#' @param data A data frame with the observed variables.
#' @param theory The theory of the model. Should be one of `"fo"` (first-order),
#'   `"ho"` (higher-order), `"bf"` (bi-factor) and `"of"` (one-factor). See
#'   Brunner et al. (2012) for detailed discussion of the models and the naming
#'   conventions used here.
#' @param ... Other arguments passed to `cfa()`.
#' @param col_ov,col_lv The name of the column in `config` that
#'   specifies the observed and latent variables.
#' @param col_fix The name of the column in `config` that specifies the fixed
#'   parameters. Only used for loadings. If `NULL`, all parameters are free.
#' @return The same as [lavaan::cfa()].
#' @references
#'
#' Brunner, M., Nagy, G., & Wilhelm, O. (2012). A Tutorial on Hierarchically
#' Structured Constructs. Journal of Personality, 80(4), 796–846.
#' https://doi.org/10.1111/j.1467-6494.2011.00749.x
#' @export
fit_cfa <- function(config, data, theory, ...,
                    col_ov = observed,
                    col_lv = latent,
                    col_fix = NULL) {
  rlang::check_dots_used()
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
    missing = "ml",
    orthogonal = theory == "bf",
    ...
  )
}

prepare_model <- function(config, theory, col_ov, col_lv,
                          col_fix = NULL) {
  no_fix <- rlang::quo_is_null(rlang::enquo(col_fix))
  config <- config |>
    rename(
      observed = {{ col_ov }},
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
        bf = "g =~ {observed}",
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
        "{latent} =~ {observed}"
      } else {
        "{latent} =~ {fix} * {observed}"
      }
    )
  }
  c(model_g, model_facts)
}

extract_latent_scores <- function(fit, data = NULL, id_cols_data = NULL) {
  scores <- as_tibble(unclass(lavPredict(fit, data)))
  if (!is.null(data)) {
    id_cols_data <- substitute(id_cols_data) %||% quote(user_id)
    scores <- bind_cols(select(data, {{ id_cols_data }}), scores)
  }
  scores
}

# Special for g factor estimation ----
prepare_config_vars <- function(num_vars_total, n_steps) {
  num_vars_base <- num_vars_total %/% n_steps
  tibble(
    num_vars = seq(num_vars_base, num_vars_total, num_vars_base),
    use_pairs = num_vars * 2 <= num_vars_total
  )
}

resample_g_scores <- function(data, num_vars, use_pairs) {
  tibble(
    num_vars = num_vars,
    use_pairs = use_pairs,
    vars = resample_vars(names(data)[-1], num_vars, use_pairs),
    g = map(vars, extract_g, data = data)
  ) |>
    mutate(id_pairs = seq_len(n()), .after = use_pairs)
}

resample_vars <- function(vars, n, use_pairs = FALSE) {
  if (use_pairs) {
    n <- n * 2
  }
  if (n > length(vars)) {
    stop("Not enough variables.")
  }
  vars_shuffled <- sample(vars, n)
  if (use_pairs) {
    idx_base <- seq_len(n / 2)
    list(
      vars_shuffled[idx_base],
      vars_shuffled[idx_base + n / 2]
    )
  } else {
    list(vars_shuffled)
  }
}

extract_g <- function(data, vars) {
  efa(
    data,
    ov.names = vars,
    std.ov = TRUE,
    std.lv = TRUE,
    missing = "ml"
  ) |>
    extract_latent_scores(data)
}
