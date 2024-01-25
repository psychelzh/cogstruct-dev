aggregate_performance <- function(cpm_result, names_to = "latent") {
  lapply(
    # targets will append batching information to the list
    zutils::select_list(cpm_result, !starts_with("tar")),
    \(result) {
      apply(result$pred, 2, cor.test, result$real) |>
        lapply(broom::tidy) |>
        list_rbind(names_to = "include")
    }
  ) |>
    list_rbind(names_to = names_to)
}
