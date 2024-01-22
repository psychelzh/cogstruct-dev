aggregate_performance <- function(cpm_result, dim_labels) {
  lapply(
    cpm_result,
    \(result) {
      apply(result$pred, 2, cor.test, result$real) |>
        lapply(broom::tidy) |>
        list_rbind(names_to = "network")
    }
  ) |>
    set_names(dim_labels) |>
    list_rbind(names_to = "dim_label")
}
