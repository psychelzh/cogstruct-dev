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
    targets,
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

tar_collect_camp <- function(contents, name_parsed = "raw_data_parsed") {
  path_archive <- Sys.getenv("OneDriveConsumer") |>
    fs::path("Documents/Research/archived/cogstruct-dev-archived")
  path_restore <- withr::with_dir(
    path_archive,
    fs::path(
      path_archive,
      tar_config_get("store", project = "preproc_behav")
    )
  )
  tarchetypes::tar_map(
    values = contents |>
      dplyr::distinct(game_id) |>
      dplyr::left_join(data.iquizoo::game_info, by = "game_id") |>
      dplyr::mutate(
        game_id = as.character(game_id),
        name_current = rlang::syms(sprintf("raw_data_%s", game_id)),
        name_restore = rlang::syms(sprintf("data_%s", game_name_abbr))
      ),
    names = game_id,
    tar_target(
      data_full,
      bind_rows(
        select(name_current, -project_id),
        possibly(read_archived)(name_restore, store = path_restore)
      ) |>
        distinct()
    ),
    tar_target_raw(name_parsed, quote(wrangle_data(data_full)))
  )
}

tar_validate_rawdata <- function(contents, name_parsed = "raw_data_parsed") {
  config_contents <- contents |>
    dplyr::distinct(game_id) |>
    dplyr::inner_join(data.iquizoo::game_info, by = "game_id") |>
    dplyr::left_join(game_data_names, by = "game_id") |>
    dplyr::mutate(
      game_id = as.character(game_id),
      require_keyboard = game_name %in% games_keyboard,
      tar_parsed = rlang::syms(sprintf("%s_%s", name_parsed, game_id))
    )
  c(
    tarchetypes::tar_map(
      values = config_contents |>
        dplyr::filter(
          !game_id %in% c(game_id_dev_err, game_id_strp, game_id_cr)
        ),
      names = game_id,
      tar_target(
        data_valid,
        validate_data(
          tar_parsed,
          require_keyboard = require_keyboard,
          list_names = list_names
        )
      )
    ),
    # correct device error
    tarchetypes::tar_map(
      values = config_contents |>
        dplyr::filter(game_id %in% game_id_dev_err),
      names = game_id,
      tar_target(
        data_valid,
        correct_device(tar_parsed) |>
          validate_data(
            require_keyboard = require_keyboard,
            list_names = list_names
          )
      )
    ),
    tarchetypes::tar_map(
      values = config_contents |>
        dplyr::filter(game_id %in% game_id_strp),
      names = game_id,
      tar_target(
        data_valid,
        validate_data(
          tar_parsed,
          require_keyboard = require_keyboard,
          list_names = list_names
        ) |>
          correct_game_dur()
      )
    ),
    # correct accuracy scores for CR
    tarchetypes::tar_map(
      values = config_contents |>
        dplyr::filter(game_id == game_id_cr),
      names = game_id,
      tar_target(
        data_valid,
        validate_data(
          tar_parsed,
          require_keyboard = require_keyboard,
          list_names = list_names
        ) |>
          correct_cr(cr_correction)
      )
    )
  )
}

tar_partition_rawdata <- function(contents, config_format, ...,
                                  name_rawdata = "data_valid",
                                  project_rawdata = NULL) {
  rlang::check_dots_empty()
  config_contents <- dplyr::distinct(contents, game_id)
  if (!is.null(project_rawdata)) {
    config_contents <- config_contents |>
      dplyr::mutate(
        tar_file_data = rlang::syms(
          stringr::str_glue("file_data_{game_id}")
        ),
        file_path = path_obj_from_proj(
          sprintf("%s_%s", name_rawdata, game_id),
          project_rawdata
        )
      )
    expr_rawdata <- quote(qs::qread(tar_file_data))
  } else {
    config_contents <- config_contents |>
      dplyr::mutate(
        tar_data = rlang::syms(
          stringr::str_glue("{name_rawdata}_{game_id}")
        )
      )
    expr_rawdata <- quote(tar_data)
  }
  config_contents <- config_contents |>
    data.iquizoo::match_preproc(type = "inner") |>
    dplyr::inner_join(config_format, by = "game_id") |>
    dplyr::filter(!is.na(format)) |>
    dplyr::mutate(
      game_id = as.character(game_id),
      prep_fun = dplyr::if_else(
        # 社交达人 needs a new prep fun for sliced versions of data
        game_id == "381576542159749",
        rlang::syms("fname_slices"),
        prep_fun
      )
    )
  c(
    if (!is.null(project_rawdata)) {
      tarchetypes::tar_eval_raw(
        substitute(
          tar_target(
            tar_file_data,
            path_obj_from_proj(
              sprintf("%s_%s", name_rawdata, game_id),
              project_rawdata
            ),
            format = "file"
          )
        ),
        values = config_contents
      )
    },
    tarchetypes::tar_map(
      config_contents |>
        dplyr::filter(format == "trials"),
      names = game_id,
      tar_target_raw(
        "indices_slices",
        substitute(
          expr_rawdata |>
            slice_data_trials(num_parts, subset = subset) |>
            preproc_data(prep_fun, .input = input, .extra = extra)
        )
      )
    ),
    tarchetypes::tar_map(
      config_contents |>
        dplyr::filter(format == "duration"),
      names = game_id,
      tar_target_raw(
        "indices_slices",
        substitute(
          expr_rawdata |>
            slice_data_duration(num_parts) |>
            preproc_data(prep_fun, .input = input, .extra = extra)
        )
      )
    ),
    tarchetypes::tar_map(
      config_contents |>
        dplyr::filter(format == "items"),
      names = game_id,
      tar_target_raw(
        "indices_slices",
        substitute(
          expr_rawdata |>
            slice_data_items() |>
            preproc_data(prep_fun, .input = input, .extra = extra)
        )
      )
    ),
    tarchetypes::tar_map(
      config_contents |>
        dplyr::filter(format == "blocks"),
      names = game_id,
      tar_target_raw(
        "indices_slices",
        substitute(
          expr_rawdata |>
            slice_data_blocks() |>
            preproc_data(prep_fun, .input = input, .extra = extra)
        )
      )
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
