library(targets)
future::plan(future.callr::callr)
tar_source()
tar_option_set(
  package = c("tidyverse", "bit64", "lavaan"),
  format = "qs",
  memory = "transient",
  garbage_collection = TRUE,
  controller = crew::crew_controller_local(workers = 8)
)

store_preproc <- fs::path(
  tar_config_get("store", project = "prepare_source_data"),
  "objects"
)

output_factcons <- function(exclude_id, mat, ...,
                            file_prefix = "factcons",
                            dir_output = ".output/factor-consistency") {
  file <- fs::path(
    dir_output,
    str_glue("{file_prefix}_exclude-{exclude_id}.png")
  )
  rownames(mat) <- replace_as_name_cn(rownames(mat))
  colnames(mat) <- replace_as_name_cn(colnames(mat))
  ragg::agg_png(file, width = 1980, height = 1980, res = 100)
  corrplot::corrplot(
    mat,
    type = "upper",
    method = "color",
    order = "hclust",
    hclust.method = "ward",
    col.lim = c(0, 1),
    col = corrplot::COL2("RdBu")
  )
  dev.off()
  file
}

games_nback <- c("Nback4", "Digit3back", "Verbal3back", "Grid2back", "Paint2back")
games_filt <- c("FiltColor", "FiltOrient")
config <- tidyr::expand_grid(
  exclude_nback = combn(games_nback, length(games_nback) - 1, simplify = FALSE),
  exclude_filt = combn(games_filt, length(games_filt) - 1, simplify = FALSE)
) |>
  dplyr::mutate(
    exclude = purrr::map2(exclude_nback, exclude_filt, c),
    include = purrr::map(exclude, ~ setdiff(c(games_nback, games_filt), .x)),
    .keep = "unused"
  ) |>
  tibble::add_row(
    exclude = list(character(), c(games_nback, games_filt), games_filt)
  ) |>
  dplyr::mutate(exclude_id = dplyr::consecutive_id(exclude), .before = 1) |>
  tidyr::expand_grid(n_fact = 4:10)

list(
  tarchetypes::tar_file_read(
    indices_wider_clean,
    fs::path(store_preproc, "indices_wider_clean"),
    read = qs::qread(!!.x)
  ),
  tarchetypes::tar_map_rep(
    fact_attribution,
    indices_wider_clean |>
      select(-user_id) |>
      resample_fact_attribution(n_fact, exclude),
    values = dplyr::select(config, -include),
    names = c(exclude_id, n_fact),
    batches = 10,
    reps = 10
  ),
  tarchetypes::tar_group_by(
    fact_attribution_groups,
    fact_attribution,
    exclude_id,
    exclude,
    n_fact
  ),
  tar_target(
    prob_one_fact,
    fact_attribution_groups |>
      mutate(
        pairs = map(
          game_index,
          ~ if (length(.x) > 1) {
            combn(.x, 2, sort, simplify = FALSE)
          }
        ),
        .keep = "unused"
      ) |>
      unnest(pairs) |>
      summarise(
        prob = n() / 100,
        .by = c(exclude_id, exclude, n_fact, pairs)
      ) |>
      mutate(
        x = purrr::map_chr(pairs, 1),
        y = purrr::map_chr(pairs, 2),
        .keep = "unused"
      ) |>
      group_by(exclude_id, exclude, n_fact) |>
      group_modify(
        ~ tibble(
          mat = {
            mat_origin <- .x |>
              select(x, y, prob) |>
              igraph::graph_from_data_frame(directed = FALSE) |>
              igraph::as_adjacency_matrix(attr = "prob") |>
              as.matrix()
            order <- sort(colnames(mat_origin))
            list(mat_origin[order, order])
          }
        )
      ) |>
      ungroup(),
    pattern = map(fact_attribution_groups)
  ),
  tar_target(
    prob_one_fact_avg,
    prob_one_fact |>
      summarise(
        mat = list(reduce(mat, `+`) / n()),
        .by = c(exclude_id, exclude)
      )
  ),
  tar_target(
    files_plots,
    prob_one_fact_avg |>
      pmap_chr(output_factcons)
  ),
  tar_target(
    prob_one_fact_large,
    prob_one_fact |>
      filter(n_fact > 7) |>
      summarise(
        mat = list(reduce(mat, `+`) / n()),
        .by = c(exclude_id, exclude)
      )
  ),
  tar_target(
    files_plots_large,
    prob_one_fact_large |>
      pmap_chr(output_factcons, file_prefix = "factcons_nfact-large")
  ),
  tarchetypes::tar_map(
    values = tibble::tibble(
      thresh_value = seq(0.4, 0.8, 0.1),
      thresh_level = seq_along(thresh_value)
    ),
    names = thresh_level,
    tar_target(
      fact_cons_bin,
      prob_one_fact_avg |>
        mutate(mat = map(mat, ~ .x > thresh_value))
    ),
    tar_target(
      files_plots_bin,
      fact_cons_bin |>
        pmap_chr(
          output_factcons,
          file_prefix = str_c("factcons_thresh-", thresh_level)
        )
    )
  ),
  tar_target(
    cluster_result,
    prob_one_fact_avg |>
      # keep "格子卡片" only considering n-back tests are too similar
      filter(exclude_id == 3) |>
      mutate(mat = map(mat, ~ 1 - .x)) |>
      pluck("mat", 1) |>
      as.dist() |>
      hclust(method = "ward.D2")
  ),
  tar_target(
    cluster_best,
    cluster_result |>
      dendextend::find_k() |>
      pluck("pamobject", "silinfo", "widths") |>
      as_tibble(rownames = "game_index") |>
      mutate(
        # at least keep 5 tasks
        include = row_number(desc(sil_width)) <= 5 |
          sil_width > mean(sil_width),
        .by = cluster
      )
  ),
  tar_target(
    file_cluster_best,
    cluster_best |>
      separate_wider_delim(
        game_index,
        delim = ".",
        names = c("game_name_abbr", "index_name"),
        cols_remove = FALSE
      ) |>
      left_join(
        select(data.iquizoo::game_info, game_name, game_name_abbr),
        by = "game_name_abbr"
      ) |>
      relocate(game_name, .before = 1L) |>
      write_excel_csv("config/factcons_clustering.csv")
  )
)
