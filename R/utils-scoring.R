#' Fit a one-g factor model for given observed variables
#'
#' @param data Raw behavior data.
#' @param vars A character vector specifying observed variables.
#' @returns A fitted one-g factor model.
#' @export
#' @import lavaan
fit_g <- function(data, vars) {
  efa(data, ov.names = vars, std.ov = TRUE, missing = "ml")
}

#' Calculate the variance explained by g factor
#'
#' @param fit A fitted one-g factor model.
#' @returns A numeric value indicating the variance explained by g factor.
#' @export
calc_var_exp <- function(fit) {
  mean(loadings(fit)^2)
}

#' Predict g factor scores
#'
#' @param data Raw behavior data.
#' @param mdl A fitted one-g factor model.
#' @param id_cols A numeric vector specifying the column indices of subject
#'   identifiers.
#' @returns A data frame with g factor scores.
#' @export
predict_g_score <- function(data, mdl, id_cols = 1) {
  g <- lavPredict(mdl)[, 1]
  data_names <- rownames(loadings(mdl))
  for (data_name in data_names) {
    test <- cor.test(g, data[[data_name]], use = "pairwise")
    # if g is anti-correlated significantly with any ov, inverse it
    if (test$estimate < 0 && test$p.value < 0.05) {
      g <- -g
      break
    }
  }
  add_column(data[, id_cols], g = g)
}

slice_data <- function(data, format, subset = NA, parts = 5,
                       name_raw_parsed = "raw_parsed") {
  if (is_empty(format)) {
    return()
  }
  cols_meta <- setdiff(names(data), name_raw_parsed)
  switch(format,
    duration = {
      data_rt_cum <- data |>
        unnest(any_of(name_raw_parsed)) |>
        mutate(rt_cum = cumsum(rt), .by = all_of(cols_meta))
      config_parts <- data_rt_cum |>
        group_by(pick(all_of(cols_meta))) |>
        slice_tail(n = 1) |>
        reframe(
          tibble(
            part = seq_len(parts - 1),
            rt_cum_break = rt_cum * part / parts
          )
        ) |>
        ungroup()
      data_rt_cum |>
        inner_join(
          config_parts,
          by = join_by(
            !!!cols_meta,
            rt_cum <= rt_cum_break
          )
        ) |>
        select(-contains("rt_cum")) |>
        nest(.by = all_of(c(cols_meta, "part")), .key = name_raw_parsed)
    },
    trials = {
      num_trials <- nrow(data[[name_raw_parsed]][[1]])
      if (!all(map_int(data[[name_raw_parsed]], nrow) == num_trials)) {
        warning("For trials format, all data must have equal number of rows.")
      }
      data_unnested <- data |>
        unnest(any_of(name_raw_parsed))
      if (!is.na(subset)) {
        data_unnested <- data_unnested |>
          filter(eval(parse(text = subset)))
      }
      config_parts <- tibble(
        part = seq_len(parts - 1),
        row_num_break = num_trials * part / parts
      )
      data_unnested |>
        mutate(row_num = row_number(), .by = all_of(cols_meta)) |>
        inner_join(
          config_parts,
          by = join_by(row_num <= row_num_break)
        ) |>
        select(-contains("row_num")) |>
        nest(.by = all_of(c(cols_meta, "part")), .key = name_raw_parsed)
    },
    items = {
      num_items <- nrow(data[[name_raw_parsed]][[1]])
      if (!all(map_int(data[[name_raw_parsed]], nrow) == num_items)) {
        warning("For trials format, all data must have equal number of rows.")
      }
      data_unnested <- data |>
        unnest(any_of(name_raw_parsed))
      if (!is.na(subset)) {
        data_unnested <- data_unnested |>
          filter(eval(parse(text = subset)))
      }
      data_unnested |>
        mutate(part = row_number(), .by = all_of(cols_meta)) |>
        nest(.by = all_of(c(cols_meta, "part")), .key = name_raw_parsed)
    }
  )
}
