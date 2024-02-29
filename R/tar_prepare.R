prepare_config_vars <- function(num_vars_total, n_steps) {
  num_vars_base <- num_vars_total %/% n_steps
  tibble::tibble(
    num_vars = seq(num_vars_base, num_vars_total, num_vars_base),
    use_pairs = num_vars * 2 <= num_vars_total
  ) |>
    dplyr::filter(choose(num_vars_total, num_vars) > 1e4)
}

prepare_config_cpm <- function(...) {
  tidyr::expand_grid(
    config_fc,
    hypers_cpm
  ) |>
    dplyr::filter(...) |>
    dplyr::mutate(
      file_fc = rlang::syms(
        sprintf(
          "file_fc_%s_%s_%s_%s_%s",
          config, session, task, atlas, run
        )
      ),
      fd = rlang::syms(
        sprintf(
          "fd_%s_%s_%s",
          session, task, run
        )
      )
    )
}

prepare_config_retest <- function(contents, name_suffix = NULL) {
  contents |>
    dplyr::distinct(game_id) |>
    data.iquizoo::match_preproc(type = "semi", rm_tagged = TRUE) |>
    dplyr::left_join(data.iquizoo::game_info, by = "game_id") |>
    dplyr::mutate(
      dplyr::across(dplyr::contains("game_id"), as.character),
      game_id_real = dplyr::coalesce(game_id_parallel, game_id)
    ) |>
    dplyr::mutate(
      tar_indices = lapply(
        game_id,
        \(game_id) {
          as.symbol(
            paste(c("indices", name_suffix, game_id), collapse = "_")
          )
        }
      )
    ) |>
    dplyr::select(game_id = game_id_real, tar_indices) |>
    tidyr::chop(tar_indices)
}
