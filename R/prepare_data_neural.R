# functional connectivity data preparation ----
prepare_data_fc <- function(ts) {
  structure(
    do.call(rbind, lapply(ts$data, calc_fc)),
    id = pull(ts, user_id, name = subject)
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
compose_confounds_cpm <- function(users_demography, fd_mean) {
  data <- users_demography |>
    select(user_id, user_sex, user_age) |>
    inner_join(fd_mean, by = join_by(user_id)) |>
    mutate(scanner = str_remove_all(subject, "\\d")) |>
    fastDummies::dummy_columns(
      "scanner",
      remove_first_dummy = TRUE,
      remove_selected_columns = TRUE
    )
  structure(
    as.matrix(select(data, !c(user_id, subject))),
    id = pull(data, user_id, name = subject)
  )
}

prepare_fd_mean <- function(confounds) {
  confounds |>
    mutate(
      fd = map_dbl(
        data,
        \(x) mean(x$framewise_displacement, na.rm = TRUE)
      ),
      .keep = "unused"
    )
}

prepare_data_confounds <- function(files) {
  files |>
    summarise(
      data = list(merge_runs(path, fill = TRUE)),
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

merge_runs <- function(files, ...) {
  dat <- lapply(files, data.table::fread, na.strings = "n/a")
  structure(
    data.table::rbindlist(dat, ...),
    rows = map_int(dat, nrow)
  )
}

calc_fc <- function(ts) {
  as.dist(atanh(cor(ts)))
}
