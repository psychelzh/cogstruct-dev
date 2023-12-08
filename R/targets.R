tar_sample_tasks <- function(num_tasks, data,
                             name_id_col = 1,
                             sample_times = 10,
                             sample_size = 100,
                             name_suffix = "") {
  cfg_rsmp_vars <- withr::with_seed(
    1,
    tidyr::expand_grid(
      num_vars = round(seq(3, floor(num_tasks / 2), length.out = sample_times)),
      idx_rsmp = seq_len(sample_size)
    ) |>
      dplyr::reframe(
        purrr::map(
          num_vars,
          ~ data.frame(
            id_pairs = rep(c(1, 2), .),
            idx_vars = sample.int(num_tasks, . * 2, replace = FALSE)
          )
        ) |>
          purrr::list_rbind(),
        .by = c(num_vars, idx_rsmp)
      ) |>
      tidyr::chop(idx_vars) |>
      tidyr::chop(c(idx_rsmp, idx_vars))
  )
  tarchetypes::tar_map(
    values = cfg_rsmp_vars,
    names = c(num_vars, id_pairs),
    tar_target_raw(
      paste0("data_names", name_suffix),
      tibble(
        idx_rsmp = idx_rsmp, # use this to track samples
        tasks = map(idx_vars, ~ names(data)[-name_id_col][.])
      ) |>
        substitute(),
      deployment = "main"
    ),
    tar_target_raw(
      paste0("mdl_fitted", name_suffix),
      .(as.name(paste0("data_names", name_suffix))) |>
        mutate(
          mdl = map(tasks, ~ fit_g(.(substitute(data)), all_of(.))),
          .keep = "unused"
        ) |>
        bquote()
    ),
    tar_target_raw(
      paste0("scores_g", name_suffix),
      .(as.name(paste0("mdl_fitted", name_suffix))) |>
        mutate(
          scores = map(mdl, ~ predict_g_score(.(substitute(data)), .)),
          .keep = "unused"
        ) |>
        bquote()
    )
  )
}

tar_combine_with_meta <- function(name, targets, cols_targets,
                                  fun_pre = NULL,
                                  fun_post = NULL) {
  ischar_name <- tryCatch(
    is.character(name) && length(name) == 1L,
    error = function(e) FALSE
  )
  if (!ischar_name) {
    name <- deparse1(substitute(name))
  }
  if (is.null(fun_pre)) {
    fun_pre <- \(x) x
  }
  if (is.null(fun_post)) {
    fun_post <- \(x) x
  }
  tarchetypes::tar_combine_raw(
    name,
    targets[[name]],
    command = bquote(
      list(!!!.x) |>
        lapply(.(rlang::as_function(fun_pre))) |>
        bind_rows(.id = "id") |>
        # note there is delimiter after name should be removed too
        mutate(id = str_remove(id, str_c(.(name), "."))) |>
        separate(id, .(cols_targets), convert = TRUE) |>
        .(rlang::as_function(fun_post))()
    )
  )
}

tar_prep_creativity <- function() {
  list(
    tarchetypes::tar_file_read(
      cr_correction,
      "data/cr_correction.parquet",
      read = arrow::read_parquet(!!.x)
    ),
    tarchetypes::tar_file_read(
      aut_grade_scores,
      "data/aut_grade_scores.parquet",
      read = arrow::read_parquet(!!.x)
    ),
    tarchetypes::tar_file_read(
      aut_grade_types,
      "data/aut_grade_types.parquet",
      read = arrow::read_parquet(!!.x)
    ),
    tarchetypes::tar_file_read(
      vg_dists,
      "data/vg_dists.parquet",
      read = arrow::read_parquet(!!.x)
    ),
    tarchetypes::tar_file_read(
      dat_w2v,
      "data/dat_w2v.qs",
      read = qs::qread(!!.x)
    )
  )
}
