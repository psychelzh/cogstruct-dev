# functional connectivity data preparation ----
prepare_meta_time_series <- function(config, session, task, atlas) {
  extract_bids_meta(
    config,
    session = session,
    task = task,
    atlas = atlas,
    suffix = "timeseries",
    extension = "tsv"
  )
}

prepare_data_fc <- function(meta, ...) {
  meta |>
    filter(...) |>
    summarise(
      fc = list(calc_fc(path)),
      .by = subject
    ) |>
    mutate(
      user_id = data.camp::users_id_mapping[subject],
      .keep = "unused",
      .before = 1L
    ) |>
    deframe() |>
    do.call(rbind, args = _)
}

# frame-wise displacement (FD) data preparation ----
prepare_meta_data_motion <- function(session, task) {
  extract_bids_meta(
    # different xcp_d results used the same motion parameters
    "gsr",
    session = session,
    task = task,
    suffix = "motion",
    extension = "tsv"
  )
}

prepare_fd_mean <- function(meta) {
  meta |>
    mutate(
      fd_mean = map_dbl(
        path,
        \(file) {
          data.table::fread(
            file,
            na.strings = "n/a"
          )$framewise_displacement[-1] |> # no fd for the first time point
            mean()
        }
      ),
      .keep = "unused"
    ) |>
    pivot_wider(
      id_cols = subject,
      names_from = run,
      values_from = fd_mean
    ) |>
    mutate(
      user_id = data.camp::users_id_mapping[subject],
      .keep = "unused",
      .before = 1L
    ) |>
    column_to_rownames("user_id") |>
    as.matrix()
}

# helper functions
extract_bids_meta <- function(config, ...) {
  path_root <- fs::path(
    Sys.getenv("ROOT_BIDS_DERIV"),
    sprintf("xcpd_%s", config)
  )
  reticulate::use_condaenv("bids")
  bids <- reticulate::import("bids")
  bids$BIDSLayout(
    fs::path(path_root, "xcp_d"),
    validate = FALSE,
    database_path = fs::path(path_root, "layout")
  )$get(...) |>
    map(
      ~ as_tibble(.x$get_entities()) |>
        add_column(path = .x$path)
    ) |>
    list_rbind()
}

calc_fc <- function(path) {
  lapply(path, data.table::fread, na.strings = "n/a") |>
    data.table::rbindlist() |>
    fisher_cor() |>
    as.dist()
}

fisher_cor <- function(ts) {
  atanh(cor(ts))
}
