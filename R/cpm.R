perform_cpm_g_factor <- function(fc, g, confounds, subjs_keep_neural, ...) {
  # remove possible missing values in g with a warning
  if (anyNA(g)) {
    warning("Found missing g factor scores, will remove them.")
    g <- g[!is.na(g), 1, drop = FALSE]
  }
  subjs_to_keep <- intersect(
    rownames(g),
    as.character(subjs_keep_neural)
  )
  cpmr::cpm(
    fc[subjs_to_keep, ],
    g[subjs_to_keep, ],
    confounds = confounds[subjs_to_keep, ],
    kfolds = 10,
    ...
  )
}

extract_cpm_performance <- function(result) {
  as_tibble(
    cor(result$pred, result$real),
    rownames = "include",
    .name_repair = ~ "r"
  )
}

match_confounds <- function(users_confounds, fd_mean) {
  subjs <- intersect(rownames(users_confounds), rownames(fd_mean))
  cbind(users_confounds[subjs, ], fd_mean[subjs, ])
}
