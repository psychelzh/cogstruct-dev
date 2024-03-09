library(targets)
tar_source()
tar_option_set(
  packages = c("tidyverse", "bit64", "lavaan"),
  format = "qs",
  controller = setup_crew_controller("fc")
)
setup_parallel_plan()

config_fc_calc <- config_fc |>
  tidyr::unite("name_suffix", everything(), remove = FALSE) |>
  dplyr::mutate(
    meta_time_series = rlang::syms(
      paste0("meta_time_series_", name_suffix)
    ),
    fc = rlang::syms(paste0("fc_", name_suffix))
  )

targets_fd <- tarchetypes::tar_map(
  dplyr::distinct(params_fmri_tasks, session, task),
  names = !runs,
  tar_target(
    meta_data_motion,
    prepare_meta_data_motion(session, task)
  ),
  tar_target(
    fd_mean,
    prepare_fd_mean(meta_data_motion)
  )
)

list(
  # prepare functional connectivity (FC) data ----
  tarchetypes::tar_eval(
    list(
      tar_target(
        meta_time_series,
        prepare_meta_time_series(xcpd, session, task, atlas, run)
      ),
      tar_target(
        fc,
        prepare_data_fc(meta_time_series, cortical_only = cortical_only)
      )
    ),
    config_fc_calc |>
      dplyr::filter(task != "latent") |>
      dplyr::mutate(
        session = lapply(session, \(x) if (x == "0") character() else x),
        run = lapply(run, parse_digits),
        cortical_only = atlas == "Schaefer200Parcels"
      )
  ),
  tarchetypes::tar_eval(
    tar_target(
      fc,
      # calculate principal component only on kept subjects
      lapply(call_list_fc, \(x) x[subjs_keep_neural, ]) |>
        abind::abind(along = 3) |>
        apply(2, \(x) princomp(x)$scores[, 1])
    ),
    config_fc_calc |>
      dplyr::filter(task == "latent") |>
      dplyr::mutate(
        call_list_fc = purrr::pmap(
          list(xcpd, atlas, run),
          \(cur_xcpd, cur_atlas, cur_run) {
            config <- config_fc_calc |>
              dplyr::filter(
                task != "latent",
                session != "0",
                xcpd == cur_xcpd,
                atlas == cur_atlas,
                if (cur_run == "full") {
                  grepl("^run\\d$", run)
                } else {
                  run == cur_run
                }
              )
            as.call(c(quote(list), config$fc))
          }
        )
      )
  ),
  # extract mean frame-wise displacement (FD) ----
  targets_fd,
  tarchetypes::tar_combine(
    fd_mean,
    targets_fd$fd_mean,
    command = do.call(cbind, list(!!!.x))
  ),
  # criteria for keeping subjects ---
  # 1. no run with mean FD > 0.5
  # 2. no missing runs
  tar_target(
    subjs_keep_neural,
    names(which(apply(fd_mean, 1, \(x) all(x <= 0.5) && all(!is.na(x)))))
  ),
  tarchetypes::tar_map(
    dplyr::distinct(params_conmat, atlas) |>
      dplyr::mutate(
        atlas_store = ifelse(
          atlas == "Schaefer200Parcels",
          "4S256Parcels",
          atlas
        )
      ),
    names = atlas,
    tar_target(
      file_atlas_dseg,
      fs::path(
        Sys.getenv("ROOT_BIDS_DERIV"),
        "xcpd_gsr",
        "xcp_d",
        "atlases",
        sprintf("atlas-%s", atlas_store)
      ) |>
        fs::dir_ls(regexp = "tsv"),
      format = "file_fast"
    ),
    tar_target(
      atlas_dseg,
      {
        result <- read_tsv(file_atlas_dseg, show_col_types = FALSE, na = "n/a")
        if (atlas == "Schaefer200Parcels") {
          result[1:200, ]
        } else {
          result
        }
      }
    )
  )
)
