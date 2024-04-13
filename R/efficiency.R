prepare_efficiency <- function(fc, weighted = TRUE, thresh_level = 0) {
  do.call(
    rbind, # ensure rows are subjects
    apply(
      # converted to correlation values
      tanh(fc), 1,
      \(x) calc_efficiency(x, weighted, thresh_level),
      simplify = FALSE
    )
  )
}

predict_efficiency <- function(efficiency, scores) {
  subjs_keep <- intersect(rownames(efficiency), rownames(scores))
  apply(efficiency[subjs_keep, ], 2, cor.test, scores[subjs_keep, ]) |>
    lapply(broom::tidy) |>
    list_rbind(names_to = "node_id")
}

# helper functions ----
calc_efficiency <- function(fc, weighted, thresh_level) {
  # assume pearson correlations (not Fisher transformed)
  if (thresh_level > 0) {
    fc <- ifelse(fc > thresh_level, fc, 0)
  }
  if (weighted) {
    fc <- proxy::pr_simil2dist(fc)
  } else {
    fc <- as.numeric(fc > 0)
  }
  graph <- Rfast::squareform(fc) |>
    igraph::graph_from_adjacency_matrix(
      mode = "undirected",
      weighted = weighted
    )
  c(
    igraph::local_efficiency(graph),
    igraph::global_efficiency(graph)
  )
}
