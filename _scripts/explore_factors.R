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

resample_fact_attribution <- function(data, n_fact, exclude = character()) {
  data |>
    select(!contains(exclude)) |>
    slice_sample(prop = 1, replace = TRUE) |>
    psych::fa(n_fact) |>
    extract_efa_params(drop_load = TRUE) |>
    chop(game_index)
}

extract_prob_one_fact <- function(fact_attribution) {
  fact_attribution |>
    complete(mr, nesting(tar_batch, tar_rep, tar_seed)) |>
    mutate(
      pairs = map(
        game_index,
        ~ expand.grid(x = .x, y = .x)
      ),
      .keep = "unused"
    ) |>
    add_count(mr, name = "num_samples") |>
    unnest(pairs) |>
    count(num_samples, x, y, name = "num_occur") |>
    mutate(prob = num_occur / num_samples, .keep = "unused") |>
    retract_tbl_to_mat(sort_names = TRUE)
}

output_factcons <- function(schema, mat, ...,
                            file_prefix = "factcons",
                            dir_output = "_output/factor-consistency") {
  file <- fs::path(
    dir_output,
    str_glue("{file_prefix}_schema-{schema}.png")
  )
  rownames(mat) <- replace_as_name_cn(rownames(mat))
  colnames(mat) <- replace_as_name_cn(colnames(mat))
  ragg::agg_png(file, width = 1980, height = 1980, res = 100)
  corrplot::corrplot(
    mat,
    type = "upper",
    method = "color",
    order = "hclust",
    hclust.method = "ward.D2",
    col.lim = c(0, 1),
    col = corrplot::COL2("RdBu")
  )
  dev.off()
  file
}

games_nback <- c(
  "Nback4", "Digit3back", "Verbal3back", "Grid2back", "Paint2back"
)
config <- tibble::tribble(
  ~schema, ~exclude,
  "all", character(),
  "nbackfree", games_nback, # exclude all N-back
  "nbackone", setdiff(games_nback, "Grid2back") # keep grid 2-back only
)

targets_fact_resamples <- tarchetypes::tar_map(
  config,
  names = -exclude,
  tarchetypes::tar_map(
    list(n_fact = 4:10),
    tarchetypes::tar_rep(
      fact_attribution,
      resample_fact_attribution(
        indices_wider_clean,
        n_fact,
        exclude
      ),
      batches = 10,
      reps = 10
    ),
    tar_target(
      prob_one_fact,
      extract_prob_one_fact(fact_attribution)
    )
  )
)

list(
  tarchetypes::tar_file_read(
    indices_wider_clean,
    path_obj_from_proj("indices_wider_clean", "prepare_source_data"),
    read = select(qs::qread(!!.x), !user_id)
  ),
  targets_fact_resamples,
  tar_combine_with_meta(
    prob_one_fact,
    select_list(targets_fact_resamples, starts_with("prob_one_fact")),
    cols_targets = c("n_fact", "schema"),
    fun_pre = \(mat) tibble(mat = list(mat))
  ),
  tar_target(
    prob_one_fact_avg,
    prob_one_fact |>
      summarise(
        mat = list(reduce(mat, `+`) / n()),
        .by = schema
      )
  ),
  tar_target(
    files_plots,
    pmap_chr(prob_one_fact_avg, output_factcons)
  ),
  tar_target(
    prob_one_fact_large,
    prob_one_fact |>
      filter(n_fact > 7) |>
      summarise(
        mat = list(reduce(mat, `+`) / n()),
        .by = schema
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
      filter(schema == "nbackone") |>
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
