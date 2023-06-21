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
predict_g_score <- function(data, mdl, id_cols = 1, name_g = "g") {
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
  add_column(data[, id_cols], "{name_g}" := g)
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

slice_data_items <- function(data, scores_g, ...,
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
  item_order <- data_unnested |>
    mutate(acc = acc == 1) |>
    left_join(scores_g, by = "user_id") |>
    summarise(
      cor = psych::biserial(g, acc)[, 1],
      .by = itemid
    ) |>
    arrange(desc(cor))
  item_dur <- data_unnested |>
    summarise(mrt = mean(rt[acc != -1]), .by = itemid)
  parts <- max(round(sum(item_dur$mrt) / 60000), 2)
  config_parts <- tibble(
    part = seq_len(parts - 1) / parts,
    rt_cum_break = sum(item_dur$mrt) * part
  ) |>
    inner_join(
      item_order |>
        left_join(item_dur, by = "itemid") |>
        mutate(rt_cum = cumsum(mrt)),
      by = join_by(rt_cum_break >= rt_cum)
    ) |>
    select(part, itemid)
  data_unnested |>
    inner_join(config_parts, by = "itemid", relationship = "many-to-many") |>
    nest(.by = all_of(c(cols_meta, "part")), .key = name_raw_parsed)
}

slice_data_blocks <- function(data, ...,
                              name_raw_parsed = "raw_parsed") {
  cols_meta <- setdiff(names(data), name_raw_parsed)
  data_unnested <- data |>
    unnest(any_of(name_raw_parsed))
  if (unique(data$game_name) == "人工语言-高级") {
    data_unnested <- data_unnested |>
      mutate(
        block = cumsum(type == "learn"),
        .by = all_of(cols_meta)
      )
  } else if (unique(data$game_name) != "欢乐餐厅PRO") {
    data_unnested <- data_unnested |>
      mutate(
        block = row_number(),
        .by = all_of(cols_meta)
      )
  }
  blocks <- unique(data_unnested$block)
  config_parts <- tibble(
    part = seq_along(blocks),
    block = accumulate(blocks, c)
  ) |>
    filter(part != max(part)) |>
    unchop(block)
  data_unnested |>
    inner_join(config_parts, by = "block", relationship = "many-to-many") |>
    nest(.by = all_of(c(cols_meta, "part")), .key = name_raw_parsed)
}

# 2-fold CVs ----
split_data <- function(indices_pool) {
  distinct(indices_pool, user_id) |>
    mutate(set = c("train", "test")[sample(row_number() %% 2 + 1)]) |>
    left_join(indices_pool, by = "user_id")
}

feature_selection <- function(data, game_durs, dimensions, scores_dim, n_each = 3) {
  data |>
    left_join(game_durs, by = c("game_name", "part")) |>
    left_join(dimensions, by = "game_index") |>
    inner_join(scores_dim, by = c("user_id", "cfa")) |>
    summarise(
      n = sum(!is.na(score_adj)),
      var_exp_total = cor(score_adj, score_dim)^2,
      .by = c(label, cfa, game_index, part, mean_dur_mins)
    ) |>
    filter(n > 200) |>
    mutate(
      var_exp_per_min = var_exp_total / mean_dur_mins,
      var_exp_crit = case_when(
        cfa %in% c("Inh", "Shift") ~ var_exp_total,
        label == "EM" & part < 1 ~ 0,
        .default = var_exp_per_min
      )
    ) |>
    arrange(desc(var_exp_crit)) |>
    distinct(cfa, game_index, .keep_all = TRUE) |>
    slice_max(
      order_by = var_exp_crit,
      n = n_each,
      by = cfa
    )
}

evaluate_selection <- function(data, features, scores_dim) {
  model <- features |>
    summarise(
      formula_str = str_c(game_index, collapse = " + "),
      .by = cfa
    ) |>
    summarise(
      spec = str_c(cfa, formula_str, sep = " =~ ", collapse = "\n")
    ) |>
    pull(spec)
  model_data <- data |>
    semi_join(features, by = c("game_index", "part")) |>
    pivot_wider(
      id_cols = user_id,
      names_from = game_index,
      values_from = score_adj
    )
  fit_dim(model, model_data) |>
    predict_dim(model_data, suffix = "_part") |>
    left_join(
      scores_dim,
      by = c("user_id", "cfa")
    ) |>
    summarise(
      var_exp = cor(score_dim_part, score_dim)^2,
      .by = cfa
    )
}

evaluate_selection_g <- function(data, features, scores_g) {
  model_data <- data |>
    semi_join(features, by = c("game_index", "part")) |>
    pivot_wider(
      id_cols = user_id,
      names_from = game_index,
      values_from = score_adj
    )
  fit_g(model_data, features$game_index) |>
    predict_g_score(model_data, mdl = _, name_g = "g_part") |>
    left_join(scores_g, by = "user_id") |>
    summarise(var_exp = cor(g, g_part)^2)
}

fit_dim <- function(model, data) {
  lavaan::cfa(
    model, data,
    std.ov = TRUE, std.lv = TRUE, missing = "ml"
  )
}

predict_dim <- function(fitted, data, suffix = "") {
  lavaan::lavPredict(fitted) |>
    unclass() |>
    as_tibble() |>
    add_column(user_id = data$user_id, .before = 1L) |>
    pivot_longer(
      -user_id,
      names_to = "cfa",
      values_to = paste0("score_dim", suffix)
    )
}
