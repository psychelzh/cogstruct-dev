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

resample_vars_domain <- function(num_domain, num_vars, use_pairs = FALSE) {
  domains_chc <- unique(index_chc_labels)
  repeat {
    domains_sel <- sample(domains_chc, num_domain)
    vars <- names(index_chc_labels)[index_chc_labels %in% domains_sel]
    num_vars_real <- if (use_pairs) num_vars * 2 else num_vars
    if (choose(length(vars), num_vars_real) > 1000) break
  }
  resample_vars(vars, num_vars, use_pairs)
}
