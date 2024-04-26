library(tidyverse)
requireNamespace("bit64", quietly = TRUE)
commands <- str_glue_data(
  read_csv("config/hddm.csv", col_types = cols(game_id = "I")),
  read_file("_scripts_hddm/fit_model_tmpl.R"),
  .envir = rlang::env(context = "camp")
) |>
  walk(
    \(command) {
      script <- tempfile(tmpdir = here::here("tmp"))
      write_file(command, script)
      command_qsub <- str_glue(read_file("qsub/hddm.qsub"))
      script_qsub <- tempfile()
      write_file(command_qsub, script_qsub)
      system(str_glue("qsub {script_qsub}"))
    }
  )
