# functional connectivity data preparation ----
prepare_meta_time_series <- function(xcpd, session, task, atlas, run) {
  path_xcpd <- fs::path(
    Sys.getenv("ROOT_BIDS_DERIV"),
    sprintf("xcpd_%s", xcpd)
  )
  if (atlas == "Schaefer200Parcels") {
    atlas <- "4S256Parcels"
  }
  extract_bids_meta(
    fs::path(path_xcpd, "xcp_d"),
    fs::path(path_xcpd, "layout"),
    session = session,
    task = task,
    atlas = atlas,
    run = run,
    suffix = "timeseries",
    extension = "tsv"
  )
}

prepare_data_fc <- function(meta, ..., cortical_only = FALSE) {
  meta |>
    filter(...) |>
    summarise(fc = list(calc_fc(path, cortical_only)), .by = subject) |>
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
    fs::path(Sys.getenv("ROOT_BIDS_DERIV"), "fmriprep"),
    fs::path(Sys.getenv("ROOT_BIDS_DERIV"), "layout_fmriprep"),
    session = session,
    task = task,
    desc = "confounds",
    extension = "tsv"
  )
}

prepare_fd_mean <- function(meta) {
  meta |>
    mutate(
      fd_mean = map_dbl(
        path,
        \(file) {
          mean(
            data.table::fread(
              file,
              na.strings = "n/a",
              select = "framewise_displacement"
            )[[1]],
            na.rm = TRUE
          )
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
extract_bids_meta <- function(path_bids, path_bids_db, ...) {
  reticulate::use_condaenv("bids")
  bids <- reticulate::import("bids")
  bids$BIDSLayout(
    path_bids,
    validate = FALSE,
    database_path = path_bids_db
  )$get(...) |>
    map(
      ~ as_tibble(.x$get_entities()) |>
        add_column(path = .x$path)
    ) |>
    list_rbind()
}

calc_fc <- function(path, cortical_only = FALSE) {
  lapply(
    path,
    \(path) {
      ts <- data.table::fread(path, na.strings = "n/a")
      if (cortical_only) {
        ts <- ts[, 1:200] # only the first 200 nodes are cortical
      }
      ts
    }
  ) |>
    data.table::rbindlist() |>
    fisher_cor() |>
    as.dist()
}

fisher_cor <- function(ts) {
  atanh(cor(ts))
}
