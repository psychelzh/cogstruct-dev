suppressPackageStartupMessages({{
  library(tidyverse)
  library(brms)
}})
source("_scripts_hddm/load_data.R")
source("_scripts_hddm/sample_model.R")
requireNamespace("bit64", quietly = TRUE)
context <- "{context}"
game_id <- "{game_id}"
effect <- "{effect}"
cat("Context: ", context, "\n")
cat("Processing: ", data.iquizoo::match_info(game_id, "game_name"), "\n")
cat("Effect: ", effect, "\n")
file_save <- fs::path(
  "data-raw",
  sprintf("hddm-%s", context),
  sprintf("game-%s_effect-%s_wiener", game_id, effect)
)
sample_fun <- switch(context,
  retest2 = sample_model2,
  retest = ,
  camp = sample_model
)
load_data(context, game_id, effect, "{key}", {rt_min}, {rt_max}) |>
  sample_fun(
    context, effect,
    iter = 1000, warmup = 500,
    file = file_save
  )
