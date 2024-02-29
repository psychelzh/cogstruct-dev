perform_cpm <- function(fc, behav, confounds, ...) {
  # ensure behav is a vector
  if (is.matrix(behav)) {
    stopifnot(ncol(behav) == 1)
    behav <- behav[, 1]
  }
  if (anyNA(behav)) {
    warning("Found missing behavioral scores, will remove them.")
    behav <- behav[!is.na(behav)]
  }
  subjs_to_keep <- intersect(names(behav), rownames(fc))
  cpmr::cpm(
    fc[subjs_to_keep, ],
    behav[subjs_to_keep],
    confounds = confounds[subjs_to_keep, ],
    ...
  )
}

perform_cpm_perm <- function(fc, behav, confounds, ...) {
  # shuffle subject labels
  rownames(fc) <- sample(rownames(fc))
  perform_cpm(fc, behav, confounds, ...)
}

extract_cpm_performance <- function(result) {
  as_tibble(
    cor(result$pred, result$real),
    rownames = "include",
    .name_repair = ~"r"
  )
}

calc_dice_pairs <- function(result, level) {
  edges_sel <- lapply(
    result[1:2],
    \(x) x$edges > level * length(unique(x$folds))
  )
  proxy::simil(
    edges_sel[[1]],
    edges_sel[[2]],
    method = "Dice",
    by_rows = FALSE
  ) |>
    diag() |>
    enframe(name = "network", value = "dice")
}

match_confounds <- function(users_confounds, fd_mean) {
  subjs <- intersect(rownames(users_confounds), rownames(fd_mean))
  cbind(users_confounds[subjs, ], fd_mean[subjs, ])
}
