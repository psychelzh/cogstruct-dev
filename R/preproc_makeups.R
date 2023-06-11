countcorrect_vr <- function(data, .by = NULL, .input = NULL, .extra = NULL) {
  data |>
    mutate(
      acc = map2_int(
        cresp, resp,
        ~ all(
          str_split(.x, ",") %in%
            str_split(.y, ",")
        )
      )
    ) |>
    summarise(
      nc = sum(acc == 1),
      .by = all_of(.by)
    )
}
