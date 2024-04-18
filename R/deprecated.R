# older method of factor exploration
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

# https://www.open-access.bcu.ac.uk/6076/
iterate_efa <- function(r, n_obs = 100) { # nolint: cyclocomp_linter.
  if (!psych::isCorrelation(r)) {
    n_obs <- nrow(r)
    r <- cor(r, use = "pairwise.complete.obs")
  }
  nfact <- psych::fa.parallel(r, n_obs, plot = FALSE)$nfact
  efa <- psych::fa(r, nfact, n_obs)
  removed <- list(
    loading_too_small = character(),
    loading_cross = character()
  )
  repeat {
    stable <- TRUE
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
