prepare_config_cpm <- function(...) {
  tidyr::expand_grid(
    params_fmri_tasks,
    params_xcpd,
    hypers_cpm
  ) |>
    dplyr::filter(...) |>
    dplyr::mutate(
      file_fc = rlang::syms(
        sprintf(
          "file_fc_%s_%s_%s_%s",
          session, task, config, atlas
        )
      ),
      file_confounds = rlang::syms(
        sprintf(
          "file_confounds_%s_%s",
          session, task
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
      game_id_rel = dplyr::coalesce(game_id_parallel, game_id)
    ) |>
    dplyr::summarise(
      tar_indices = list(
        lapply(
          game_id,
          \(game_id) {
            as.symbol(
              paste(c("indices", name_suffix, game_id), collapse = "_")
            )
          }
        )
      ),
      .by = game_id_rel
    ) |>
    dplyr::rename(game_id = game_id_rel)
}
