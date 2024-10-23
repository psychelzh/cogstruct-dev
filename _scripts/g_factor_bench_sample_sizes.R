library(targets)
tar_source()
tar_option_set(
  packages = c("tidyverse", "bit64", "lavaan"),
  format = "qs",
  controller = setup_crew_controller("gf_samples")
)

# configure data
data_root <- r"(D:\Research\brain-intelligence\g-factor)"
targets_rsmp <- tarchetypes::tar_map(
  expand.grid(
    id = seq_len(100),
    size = c(100, 200, 300, 400, 500)
  ),
  tar_target(
    subjs_keep_rsmp,
    sample(subjs_keep, size)
  ),
  tarchetypes::tar_rep(
    cpm_result,
    cpmr::cpm(
      fc[subjs_keep_rsmp, ],
      scores_g[subjs_keep_rsmp, ],
      confounds = users_confounds[subjs_keep_rsmp, ],
      kfolds = 10
    ),
    batches = 4,
    reps = 5,
    iteration = "list",
    retrieval = "worker",
    storage = "worker"
  ),
  tarchetypes::tar_rep2(
    cpm_performance_cv,
    extract_cpm_performance(cpm_result),
    cpm_result,
    retrieval = "worker",
    storage = "worker"
  ),
  tar_target(
    cpm_performance,
    summarise(cpm_performance_cv, r = mean(r), .by = include)
  )
)

list(
  tarchetypes::tar_file_read(
    fc,
    fs::path(
      data_root, "data", "neural",
      "cond-nbackrun1_parcel-nn268_gsr-with_acq-orig_fc.arrow"
    ),
    read = arrow::read_feather(!!.x) |>
      column_to_rownames("sub_id") |>
      as.matrix()
  ),
  tarchetypes::tar_file_read(
    users_confounds,
    fs::path(
      data_root,
      withr::with_dir(
        data_root,
        path_obj_from_proj("subjs_covariates", "project_preproc_behav")
      )
    ),
    read = qs::qread(!!.x) |>
      select(!c(site, mean_fd_rest)) |>
      mutate(sex = as.numeric(factor(sex))) |>
      column_to_rownames("sub_id") |>
      as.matrix()
  ),
  tarchetypes::tar_file_read(
    scores_g,
    fs::path(
      data_root,
      withr::with_dir(
        data_root,
        path_obj_from_proj("scores_spearman", "project_preproc_behav")
      )
    ),
    read = qs::qread(!!.x) |>
      column_to_rownames("sub_id") |>
      as.matrix()
  ),
  tarchetypes::tar_file_read(
    subjs_keep,
    fs::path(data_root, "data", "subjs_combined"),
    read = read_lines(!!.x)
  ),
  targets_rsmp,
  tarchetypes::tar_combine(
    cpm_performance_rsmp,
    targets_rsmp$cpm_performance,
    command = bind_rows(!!!.x, .id = ".id") |>
      zutils::separate_wider_dsv(
        ".id", c("id", "size"),
        prefix = "cpm_performance"
      )
  ),
  tarchetypes::tar_rep(
    cpm_result,
    cpmr::cpm(
      fc[subjs_keep, ],
      scores_g[subjs_keep, ],
      confounds = users_confounds[subjs_keep, ],
      kfolds = 10
    ),
    batches = 4,
    reps = 5,
    iteration = "list",
    retrieval = "worker",
    storage = "worker"
  ),
  tarchetypes::tar_rep2(
    cpm_performance_cv,
    extract_cpm_performance(cpm_result),
    cpm_result,
    retrieval = "worker",
    storage = "worker"
  ),
  tar_target(
    cpm_performance,
    summarise(cpm_performance_cv, r = mean(r), .by = include)
  )
)
