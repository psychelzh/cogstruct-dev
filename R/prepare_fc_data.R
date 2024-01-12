prepare_fc_data <- function(ts) {
  structure(
    do.call(rbind, lapply(ts$data, calc_fc)),
    id = select(ts, !data)
  )
}

prepare_ts_merged <- function(ts_files) {
  ts_files |>
    summarise(
      data = lapply(path, data.table::fread) |>
        data.table::rbindlist() |>
        list(),
      .by = subject
    ) |>
    mutate(
      user_id = data.camp::users_id_mapping[subject],
      .before = 1L
    )
}

prepare_ts_files <- function(config, session, task, atlas) {
  reticulate::use_condaenv("bids")
  bids <- reticulate::import("bids")
  path <- fs::path(
    Sys.getenv("ROOT_BIDS"),
    sprintf("xcpd_%s", config)
  )
  bids$BIDSLayout(
    fs::path(path, "xcp_d"),
    validate = FALSE,
    database_path = fs::path(path, "layout")
  )$get(
    session = session,
    task = task,
    atlas = atlas,
    suffix = "timeseries",
    extension = "tsv"
  ) |>
    map(
      ~ as_tibble(.x$get_entities()) |>
        add_column(path = .x$path)
    ) |>
    list_rbind()
}

calc_fc <- function(ts) {
  as.dist(atanh(cor(ts)))
}
