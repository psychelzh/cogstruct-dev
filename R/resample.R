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

resample_vars_domain <- function(vars_domain, num_domain, num_vars, use_pairs) {
  stopifnot(
    "Cannot sample more domains than given number of variables." =
      num_domain <= num_vars
  )
  repeat {
    # vars_domain: names are domains, values are variables
    domains_pool <- sample(unique(names(vars_domain)), num_domain)
    vars_pool <- vars_domain[names(vars_domain) %in% domains_pool]
    if (choose(length(vars_pool), num_vars) < 200) next
    if (use_pairs && any(table(names(vars_pool)) < 2)) next
    vars_sel <- resample_keep_domains(vars_pool, num_vars, num_domain)
    if (!use_pairs) {
      return(list(vars_sel))
    } else {
      vars_remain <- vars_pool[!vars_pool %in% vars_sel]
      if (choose(length(vars_remain), num_vars) < 200) next
      if (n_distinct(names(vars_remain)) < num_domain) next
      return(
        list(
          vars_sel,
          resample_keep_domains(vars_domain, num_vars, num_domain)
        )
      )
    }
  }
}

resample_keep_domains <- function(vars, num_vars, num_domain) {
  repeat {
    vars_sel <- sample(vars, num_vars)
    if (n_distinct(names(vars_sel)) == num_domain) break
  }
  vars_sel
}
