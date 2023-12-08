# Support for each type of data slicing ----
slice_data_trials <- function(data, parts, ...,
                              subset = NA,
                              name_raw_parsed = "raw_parsed") {
  if (!is.na(subset)) {
    data[[name_raw_parsed]] <- map(
      data[[name_raw_parsed]],
      ~ .x |>
        filter(eval(parse(text = subset)))
    )
  }
  cols_meta <- setdiff(names(data), name_raw_parsed)
  num_trials <- nrow(data[[name_raw_parsed]][[1]])
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
  data |>
    unnest(any_of(name_raw_parsed)) |>
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

slice_data_items <- function(data, crit, ...,
                             name_raw_parsed = "raw_parsed") {
  cols_meta <- setdiff(names(data), name_raw_parsed)
  # 远距离联想 has redundant items
  if (unique(data$game_id) %in% "411281158706373") {
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
    left_join(crit, by = "user_id") |>
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
  # 人工语言-高级 needs additional step to create blocks
  if (unique(data$game_id) == "384311706735365") {
    data_unnested <- data_unnested |>
      mutate(
        block = cumsum(type == "learn"),
        .by = all_of(cols_meta)
      )
  }
  if (!has_name(data_unnested, "block")) {
    if (has_name(data_unnested, "phase")) {
      data_unnested$block <- data_unnested$phase
    } else {
      data_unnested <- data_unnested |>
        mutate(
          block = row_number(),
          .by = all_of(cols_meta)
        )
    }
  }
  blocks <- unique(data_unnested$block)
  config_parts <- tibble(
    part = seq_along(blocks) / length(blocks),
    block = accumulate(blocks, c)
  ) |>
    filter(part != 1) |>
    unchop(block)
  data_unnested |>
    inner_join(config_parts, by = "block", relationship = "many-to-many") |>
    nest(.by = all_of(c(cols_meta, "part")), .key = name_raw_parsed)
}
