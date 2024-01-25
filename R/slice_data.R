#' Partition data into roughly equal parts
#'
#' Data partitioning is essential for further task duration calibration, but
#' there are different data format to partition. Here we use different functions
#' to complete this goal.
#'
#' @name slice_data
NULL

#' @describeIn slice_data For trials format: partition into parts with equal
#'   number of trials (roughly one-minute).
#' @export
slice_data_trials <- function(data, num_parts) {
  data |>
    mutate(
      parts = map(
        raw_parsed,
        ~ tibble(
          part = seq_len(num_parts - 1) / num_parts,
          row_num_break = nrow(.x) * part
        ) |>
          inner_join(
            mutate(.x, row_num = row_number()),
            by = join_by(row_num_break >= row_num)
          ) |>
          select(-contains("row_num")) |>
          nest(.by = part, .key = "raw_parsed")
      ),
      .keep = "unused"
    ) |>
    unnest(parts)
}

#' @describeIn slice_data For equal duration format: partition into parts with
#'   equal duration (roughly one-minute).
#' @export
slice_data_duration <- function(data, num_parts) {
  data |>
    mutate(
      parts = map(
        raw_parsed,
        ~ tibble(
          part = seq_len(num_parts - 1) / num_parts,
          rt_cum_break = sum(.x$rt) * part
        ) |>
          inner_join(
            mutate(.x, rt_cum = cumsum(rt)),
            by = join_by(rt_cum_break >= rt_cum)
          ) |>
          select(-contains("rt_cum")) |>
          nest(.by = part, .key = "raw_parsed")
      ),
      .keep = "unused"
    ) |>
    unnest(parts)
}

#' @describeIn slice_data For item-based format: Item means one separate
#'   question. This function will do a basic item analysis to reorder the items
#'   based on the item discrimination.
#' @export
slice_data_items <- function(data, num_parts) {
  # 远距离联想 has redundant items
  if (unique(data$game_id) %in% "411281158706373") {
    data$raw_parsed <- map(
      data$raw_parsed,
      ~ filter(., itemid != "268009865429099")
    )
  }
  stopifnot(
    "For items format, all data must have equal number of items." =
      n_distinct(map_int(data$raw_parsed, nrow)) == 1
  )
  item_order <- data |>
    mutate(id = row_number()) |>
    tidytable::unnest(raw_parsed) |>
    filter(acc != -1) |>
    pivot_wider(
      id_cols = id,
      names_from = itemid,
      values_from = acc
    ) |>
    column_to_rownames("id") |>
    psych::alpha() |>
    pluck("item.stats") |>
    arrange(desc(r.drop)) |> # this is item discrimination
    rownames()
  data |>
    mutate(
      raw_parsed = map(
        raw_parsed,
        ~ .x[match(item_order, .x$itemid), ]
      )
    ) |>
    slice_data_trials(num_parts)
}

#' @describeIn slice_data For data with explicit blocks: Directly partition into
#'   different blocks.s
#' @export
slice_data_blocks <- function(data) {
  # add block info if not found
  data_names <- colnames(data$raw_parsed[[1]])
  add_block <- function(.x) {
    if (unique(data$game_id) == "384311706735365") {
      mutate(.x, block = cumsum(type == "learn"))
    } else if ("phase" %in% data_names) {
      rename(.x, block = phase)
    } else {
      mutate(.x, block = row_number())
    }
  }
  if (!"block" %in% data_names) {
    data$raw_parsed <- map(
      data$raw_parsed,
      add_block
    )
  }
  data |>
    mutate(
      parts = map(
        raw_parsed,
        ~ {
          blocks <- unique(.x$block)
          tibble(
            part = seq_along(blocks) / length(blocks),
            block = accumulate(blocks, c)
          ) |>
            filter(part != 1) |>
            unchop(block) |>
            inner_join(
              .x,
              by = join_by(block),
              relationship = "many-to-many"
            ) |>
            nest(.by = part, .key = "raw_parsed")
        }
      ),
      .keep = "unused"
    ) |>
    unnest(parts)
}
