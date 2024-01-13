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

games_thin <- with(
  readr::read_tsv(
    "config/games_thin.tsv",
    show_col_types = FALSE
  ),
  sort(game_name_abbr[thin])
)

config <- tibble::tribble(
  ~schema, ~exclude,
  "all", character(),
  "thin", games_thin
)
range_n_fact <- 4:20

targets_fact_resamples <- tarchetypes::tar_map(
  config,
  names = -exclude,
  tarchetypes::tar_map(
    list(n_fact = range_n_fact),
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
  tarchetypes::tar_map(
    config,
    names = -exclude,
    tar_target(
      n_factors_test,
      indices_wider_clean |>
        select(!contains(exclude)) |>
        parameters::n_factors(rotation = "oblimin")
    )
  ),
  targets_fact_resamples,
  tarchetypes::tar_combine(
    prob_one_fact,
    zutils::select_list(targets_fact_resamples, starts_with("prob_one_fact")),
    command = list(!!!.x) |>
      map(\(mat) tibble(mat = list(mat))) |>
      bind_rows(.id = ".id") |>
      zutils::separate_wider_dsv(
        ".id",
        c("n_fact", "schema"),
        prefix = "prob_one_fact"
      )
  ),
  tar_target(
    prob_one_fact_avg,
    prob_one_fact |>
      summarise(
        mat = list(do.call(matsbyname::mean_byname, mat)),
        .by = schema
      )
  ),
  tar_target(
    files_plots,
    pmap_chr(prob_one_fact_avg, output_factcons)
  ),
  tarchetypes::tar_map(
    values = tibble::tibble(
      thresh_value = seq(40, 80, 10),
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
      mutate(
        cluster = map(
          mat,
          \(mat) hclust(as.dist(100 - mat), method = "ward.D2")
        ),
        best_k = map(
          cluster,
          dendextend::find_k,
          krange = range_n_fact
        ),
        silinfo = map(
          best_k,
          \(best_k) {
            best_k |>
              pluck("pamobject", "silinfo", "widths") |>
              as_tibble(rownames = "game_index")
          }
        ),
        .keep = "unused"
      )
  ),
  tar_target(
    file_cluster_silinfo,
    cluster_result |>
      mutate(
        silinfo = lapply(
          silinfo,
          \(x) {
            x |>
              mutate(game_index = replace_as_name_cn(game_index)) |>
              separate_wider_delim(
                game_index,
                delim = ".",
                names = c("game_name", "index_name")
              )
          }
        )
      ) |>
      pull(silinfo, name = schema) |>
      writexl::write_xlsx("config.local/silinfo.xlsx")
  )
)
