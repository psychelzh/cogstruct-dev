# functional connectivity data preparation ----
prepare_data_fc <- function(ts) {
  structure(
    do.call(rbind, lapply(ts$data, calc_fc)),
    id = select(ts, !data)
  )
}

prepare_ts_merged <- function(files) {
  files |>
    summarise(
      data = list(merge_runs(path)),
      .by = subject
    ) |>
    mutate(
      user_id = data.camp::users_id_mapping[subject],
      .before = 1L
    )
}

prepare_files_ts <- function(config, session, task, atlas) {
  path_config <- fs::path(
    Sys.getenv("ROOT_BIDS_DERIV"),
    sprintf("xcpd_%s", config)
  )
  extract_bids_files(
    fs::path(path_config, "xcp_d"),
    fs::path(path_config, "layout"),
    session = session,
    task = task,
    atlas = atlas,
    suffix = "timeseries",
    extension = "tsv"
  )
}

# confounds data preparation ----
prepare_data_confounds <- function(files) {
  files |>
    summarise(
      data = list(merge_runs(path)),
      .by = subject
    ) |>
    mutate(
      user_id = data.camp::users_id_mapping[subject],
      .before = 1L
    )
}

prepare_files_confounds <- function(session, task) {
  extract_bids_files(
    fs::path(Sys.getenv("ROOT_BIDS_DERIV"), "fmriprep"),
    fs::path(Sys.getenv("ROOT_BIDS_DERIV"), "layout_fmriprep"),
    session = session,
    task = task,
    desc = "confounds",
    extension = "tsv"
  )
}

# helper functions
calc_fc <- function(ts) {
  as.dist(atanh(cor(ts)))
}

extract_bids_files <- function(path_bids, path_db, ...) {
  reticulate::use_condaenv("bids")
  bids <- reticulate::import("bids")
  bids$BIDSLayout(
    path_bids,
    validate = FALSE,
    database_path = path_db
  )$get(...) |>
    map(
      ~ as_tibble(.x$get_entities()) |>
        add_column(path = .x$path)
    ) |>
    list_rbind()
}

merge_runs <- function(files) {
  dat <- lapply(files, data.table::fread)
  structure(
    tryCatch(
      data.table::rbindlist(dat),
      error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("fill=TRUE", msg)) {
          warning(msg)
          data.table::rbindlist(dat, fill = TRUE)
        } else {
          rlang::abort(parent = e)
        }
      }
    ),
    rows = map_int(dat, nrow)
  )
}
