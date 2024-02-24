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
