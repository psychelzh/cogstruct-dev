prepare_config_vars <- function(num_vars_total, ...,
                                step = 3, from = 3, use_pairs = TRUE) {
  out <- tibble::tibble(num_vars = seq(from, num_vars_total, step)) |>
    dplyr::filter(choose(num_vars_total, num_vars) > 1e4)
  if (use_pairs) {
    out$use_pairs <- out$num_vars * 2 <= num_vars_total
  }
  out
}

prepare_config_cpm_data <- function(...) {
  config_fc |>
    dplyr::filter(...) |>
    tidyr::unite("name_suffix_fc", everything(), remove = FALSE) |>
    tidyr::unite("name_suffix_fd", c(session, task, run), remove = FALSE) |>
    dplyr::mutate(
      file_fc = rlang::syms(sprintf("file_fc_%s", name_suffix_fc)),
      fd = rlang::syms(sprintf("fd_%s", name_suffix_fd)),
      file_atlas_dseg = rlang::syms(sprintf("file_atlas_dseg_%s", atlas))
    ) |>
    dplyr::select(!tidyselect::starts_with("name_suffix"))
}

prepare_config_retest <- function(contents, name_suffix = NULL) {
  name_suffix <- if (is.null(name_suffix)) "" else paste0("_", name_suffix)
  contents |>
    dplyr::distinct(game_id) |>
    data.iquizoo::merge_preproc(filter_only = TRUE, rm_tagged = TRUE) |>
    dplyr::left_join(data.iquizoo::game_info, by = "game_id") |>
    dplyr::mutate(
      dplyr::across(dplyr::contains("game_id"), as.character),
      game_id_real = dplyr::coalesce(game_id_parallel, game_id)
    ) |>
    dplyr::select(game_id_real, game_id) |>
    tidyr::chop(game_id)
}
