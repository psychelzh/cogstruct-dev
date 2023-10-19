# Created by tarflow.iquizoo::use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tidyverse", "tarflow.iquizoo", "preproc.iquizoo"),
  imports = "preproc.iquizoo", # comment out this if only "scores" are required
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 8 workers which will run as local R processes:
  controller = crew::crew_controller_local(workers = 8)
  #
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed.

output_factcons <- function(mat, file_prefix = "factcons", ...) {
  file <- fs::path(
    ".output",
    str_glue("{file_prefix}.png")
  )
  ragg::agg_png(file, width = 1080, height = 1080, res = 100)
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

# Replace the target list below with your own:
list(
  tarflow.iquizoo::prepare_fetch_data(
    data.frame(),
    what = "raw_data", # change to "scores" or "raw_data" if you want to
    # For advanced usage, set custom templates by uncommenting next line
    templates = tarflow.iquizoo::setup_templates(
      contents = "sql/contents_short.sql"
    ),
    check_progress = FALSE # set as `FALSE` if projects finalized
  ),
  # more targets goes here
  tar_target(
    indices_clean,
    clean_indices_short(indices, contents)
  ),
  tar_target(
    indices_selected,
    indices_clean |>
      pivot_wider(
        id_cols = c(user_id, course_period),
        names_from = game_name,
        values_from = score
      ) |>
      filter(course_period == 7) |>
      naniar::add_prop_miss(-user_id, -course_period) |>
      filter(prop_miss_vars < 0.5) |>
      select(where(\(x) mean(is.na(x)) < 0.2)) |>
      select(-user_id, -course_period, -prop_miss_vars) |>
      mutate(
        across(
          everything(),
          \(x) {
            if_else(
              performance::check_outliers(x),
              NA_real_, x
            )
          }
        )
      )
  ),
  tarchetypes::tar_map_rep(
    fact_attribution,
    resample_fact_attribution(indices_selected, n_fact),
    values = data.frame(n_fact = 4:10),
    batches = 10,
    reps = 10
  ),
  tarchetypes::tar_group_by(
    fact_attribution_groups,
    fact_attribution,
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
      unnest(pairs)|>
      summarise(
        prob = n() / 100,
        .by = c(n_fact, pairs)
      ) |>
      mutate(
        x = purrr::map_chr(pairs, 1),
        y = purrr::map_chr(pairs, 2),
        .keep = "unused"
      ) |>
      group_by(n_fact) |>
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
        mat = list(reduce(mat, `+`) / n())
      )
  ),
  tar_target(
    files_plots,
    prob_one_fact_avg |>
      pmap_chr(output_factcons, file_prefix = "factcons_acq-short")
  )
)
