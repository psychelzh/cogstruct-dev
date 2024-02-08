# older method of factor exploration
resample_fact_attribution <- function(data, n_fact, exclude = character()) {
  data |>
    select(!contains(exclude)) |>
    slice_sample(prop = 1, replace = TRUE) |>
    psych::fa(n_fact) |>
    parameters::model_parameters(threshold = "max") |>
    pivot_longer(
      starts_with("MR"),
      names_to = "mr",
      values_to = "loading",
      values_drop_na = TRUE
    ) |>
    select(mr, game_index = Variable) |>
    chop(game_index)
}

extract_prob_one_fact <- function(fact_attribution) {
  fact_attribution |>
    mutate(
      pairs = map(
        game_index,
        ~ expand.grid(x = .x, y = .x)
      ),
      .keep = "unused"
    ) |>
    unnest(pairs) |>
    xtabs(~ x + y, data = _)
}

output_factcons <- function(schema, mat, ...,
                            file_prefix = "factcons",
                            dir_output = "_output/factor-consistency") {
  file <- fs::path(
    dir_output,
    str_glue("{file_prefix}_schema-{schema}.png")
  )
  rownames(mat) <- replace_as_name_cn(rownames(mat))
  colnames(mat) <- replace_as_name_cn(colnames(mat))
  ragg::agg_png(file, width = 1980, height = 1980, res = 100)
  corrplot::corrplot(
    mat,
    type = "upper",
    method = "color",
    order = "hclust",
    hclust.method = "ward.D2",
    is.corr = FALSE
  )
  dev.off()
  file
}
