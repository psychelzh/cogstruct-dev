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
  repeat {
    domains_sel <- sample(unique(index_chc_labels), num_domain)
    vars <- names(index_chc_labels)[index_chc_labels %in% domains_sel]
    if (choose(length(vars), num_vars) < 1000) next
    vars_sampled <- resample_keep_domains(vars, num_vars, num_domain)
    if (!use_pairs) {
      return(list(vars_sampled))
    } else {
      vars_remain <- setdiff(vars, vars_sampled)
      if (choose(length(vars_remain), num_vars) < 1000) next
      if (n_distinct(index_chc_labels[vars_remain]) < num_domain) next
      return(
        list(
          vars_sampled,
          resample_keep_domains(vars_remain, num_vars, num_domain)
        )
      )
    }
  }
}

resample_keep_domains <- function(vars, num_vars, num_domain) {
  num_domain <- if (num_domain > num_vars) num_vars else num_domain
  repeat {
    vars_sampled <- sample(vars, num_vars)
    if (n_distinct(index_chc_labels[vars_sampled]) == num_domain) break
  }
  vars_sampled
}
