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

resample_vars_domain <- function(num_domain, num_vars, use_pairs,
                                 label = label_chc_merge) {
  index_labels <- dplyr::pull(game_index_dims, {{ label }}, game_index)
  repeat {
    domains_sel <- sample(unique(index_labels), num_domain)
    vars <- names(index_labels)[index_labels %in% domains_sel]
    if (choose(length(vars), num_vars) < 200) next
    if (use_pairs && any(table(index_labels[vars]) < 2)) next
    vars_sel <- resample_keep_domains(vars, num_vars, num_domain, index_labels)
    if (!use_pairs) {
      return(list(vars_sel))
    } else {
      vars_remain <- setdiff(vars, vars_sel)
      if (choose(length(vars_remain), num_vars) < 200) next
      if (n_distinct(index_labels[vars_remain]) < num_domain) next
      return(
        list(
          vars_sel,
          resample_keep_domains(vars_remain, num_vars, num_domain, index_labels)
        )
      )
    }
  }
}

resample_keep_domains <- function(vars, num_vars, num_domain, index_labels) {
  num_domain <- if (num_domain > num_vars) num_vars else num_domain
  repeat {
    vars_sel <- sample(vars, num_vars)
    if (n_distinct(index_labels[vars_sel]) == num_domain) break
  }
  vars_sel
}
