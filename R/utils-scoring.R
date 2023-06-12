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

# Support for each type of data slicing ----
slice_data_trials <- function(data, parts, ...,
                              subset = NA,
                              name_raw_parsed = "raw_parsed") {
  cols_meta <- setdiff(names(data), name_raw_parsed)
  num_trials <- nrow(data[[name_raw_parsed]][[1]])
  data_unnested <- data |>
    unnest(any_of(name_raw_parsed))
  if (!is.na(subset)) {
    data_unnested <- data_unnested |>
      filter(eval(parse(text = subset)))
  }
  if (all(map_int(data[[name_raw_parsed]], nrow) == num_trials)) {
    config_parts <- tibble(
      part = seq_len(parts - 1) / parts,
      row_num_break = num_trials * part
    )
    by <- join_by(row_num <= row_num_break)
  } else {
    warning("For trials format, all data must have equal number of rows.")
    config_parts <- data |>
      group_by(pick(all_of(cols_meta))) |>
      summarise(
        num_trials = map_int(.data[[name_raw_parsed]], nrow),
        .groups = "keep"
      ) |>
      reframe(
        map(
          num_trials,
          ~ tibble(
            part = seq_len(parts - 1) / parts,
            row_num_break = .x * part
          )
        ) |>
          list_rbind()
      ) |>
      ungroup()
    by <- join_by(!!!cols_meta, row_num <= row_num_break)
  }
  data_unnested |>
    mutate(row_num = row_number(), .by = all_of(cols_meta)) |>
    inner_join(config_parts, by = by) |>
    select(-contains("row_num")) |>
    nest(.by = all_of(c(cols_meta, "part")), .key = name_raw_parsed)
}

slice_data_duration <- function(data, parts, ...,
                                subset = NA,
                                name_raw_parsed = "raw_parsed") {
  cols_meta <- setdiff(names(data), name_raw_parsed)
  data_rt_cum <- data |>
    unnest(any_of(name_raw_parsed)) |>
    mutate(rt_cum = cumsum(rt), .by = all_of(cols_meta))
  config_parts <- data_rt_cum |>
    group_by(pick(all_of(cols_meta))) |>
    slice_tail(n = 1) |>
    reframe(
      tibble(
        part = seq_len(parts - 1) / parts,
        rt_cum_break = rt_cum * part
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
}

slice_data_items <- function(data, ...,
                             name_raw_parsed = "raw_parsed") {
  cols_meta <- setdiff(names(data), name_raw_parsed)
  if (unique(data$game_name) %in% "远距离联想") {
    data[[name_raw_parsed]] <- map(
      data[[name_raw_parsed]],
      ~ filter(., itemid != "268009865429099")
    )
  }
  num_items <- nrow(data[[name_raw_parsed]][[1]])
  if (!all(map_int(data[[name_raw_parsed]], nrow) == num_items)) {
    warning("For trials format, all data must have equal number of rows.")
  }
  data_unnested <- data |>
    unnest(any_of(name_raw_parsed))
  item_dur <- data_unnested |>
    summarise(mrt = mean(rt[acc != -1]), .by = itemid) |>
    mutate(rt_cum = cumsum(mrt))
  parts <- max(round(last(item_dur$rt_cum) / 60000), 2)
  config_parts <- tibble(
    part = seq_len(parts - 1) / parts,
    rt_cum_break = last(item_dur$rt_cum) * part
  ) |>
    inner_join(item_dur, by = join_by(rt_cum_break >= rt_cum)) |>
    select(part, itemid)
  data_unnested |>
    inner_join(config_parts, by = "itemid", relationship = "many-to-many") |>
    nest(.by = all_of(c(cols_meta, "part")), .key = name_raw_parsed)
}
