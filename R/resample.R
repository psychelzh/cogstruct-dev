resample_vars <- function(vars, num_vars, use_pairs = FALSE) {
  if (use_pairs) num_vars <- num_vars * 2
  if (num_vars > length(vars)) {
    stop("Not enough variables.")
  }
  vars_sampled <- sample(vars, num_vars)
  if (use_pairs) {
    idx_base <- seq_len(num_vars / 2)
    list(
      vars_sampled[idx_base],
      vars_sampled[idx_base + num_vars / 2]
    )
  } else {
    list(vars_sampled)
  }
}

resample_pairs_chc <- function() {
  labels <- sample(unique(index_chc_labels))
  half <- length(labels) / 2
  list(labels[seq_len(half)], labels[seq_len(half) + half]) |>
    lapply(
      \(labels) {
        names(index_chc_labels)[index_chc_labels %in% labels]
      }
    )
}

allocate_num_vars_chc <- function(vars) {
  num_vars_max <- min(lengths(vars))
  num_vars <- seq(3, num_vars_max, by = 3)
  num_vars[choose(num_vars_max, num_vars) > 1000]
}
