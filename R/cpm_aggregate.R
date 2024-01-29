aggregate_performance <- function(cpm_result, names_to = "latent") {
  lapply(
    # targets will append batching information to the list
    zutils::select_list(cpm_result, !starts_with("tar")),
    \(result) {
      apply(result$pred, 2, cor.test, result$real) |>
        lapply(broom::tidy) |>
        list_rbind(names_to = "include")
    }
  ) |>
    list_rbind(names_to = names_to)
}

perform_cpm_g_factor <- function(g, file_fc, file_confounds, file_subjs_neural,
                                 thresh_method, thresh_level) {
  # remove possible missing values in g with a warning
  if (anyNA(g)) {
    warning("Found missing g factor scores, will remove them.")
    g <- g[!is.na(g), 1, drop = FALSE]
  }
  subjs_to_keep <- intersect(
    rownames(g),
    as.character(qs::qread(file_subjs_neural))
  )
  cpmr::cpm(
    qs::qread(file_fc)[subjs_to_keep, ],
    g[subjs_to_keep, ],
    confounds = qs::qread(file_confounds)[subjs_to_keep, ],
    thresh_method = thresh_method,
    thresh_level = thresh_level,
    kfolds = 10
  )
}
