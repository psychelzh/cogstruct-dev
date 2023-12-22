# The separate scores for face and vocation cannot be calculated
fname_slices <- function(data, .by = NULL, .input = NULL, .extra = NULL) {
  data |>
    summarise(fntotal = sum(acc == 1), .by = all_of(.by))
}
