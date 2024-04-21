#' Calculate global efficiency of a graph
#'
#' @param fc A functional connectivity matrix.
#' @param ... Currently not used.
#' @param weighted Whether to use weighted or binary graph.
#' @param thresh_prop Proportion of edges to keep.
#' @param negatives Whether to keep negative edges. If set to `FALSE`, all
#'   negative edges will be set to 0. If set to `TRUE`, all negative edges will
#'   be converted to positive values.
#' @param local Whether to calculate local efficiency instead of global
#'   efficiency.
#' @return A numeric value representing the efficiency of the graph.
#' @export
#' @noRd
calc_efficiency <- function(fc, ...,
                            weighted = TRUE,
                            thresh_prop = 0,
                            negatives = FALSE,
                            local = FALSE) {
  if (!negatives) {
    fc <- ifelse(fc < 0, 0, fc)
  } else {
    fc <- abs(fc)
  }
  if (thresh_prop > 0) {
    r_thresh <- quantile(fc, 1 - thresh_prop)
    fc <- ifelse(fc > r_thresh, fc, 0)
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
  if (local) {
    igraph::local_efficiency(graph)
  } else {
    igraph::global_efficiency(graph)
  }
}
