suppressPackageStartupMessages({{
  library(tidyverse)
  library(brms)
}})
source("_scripts_hddm/utils.R")
requireNamespace("bit64", quietly = TRUE)
context <- "{context}"
game_id <- "{game_id}"
effect <- "{effect}"
model_name <- if (effect == "simple") "simple" else "diff"
model <- readRDS(
  fs::path("data", sprintf("model_%s_%s.rds", model_name, context))
)
cat("Processing: ", data.iquizoo::match_info(game_id, "game_name"), "\n")
cat("Context: ", context, "\n")
file_save <- fs::path(
  "data-raw",
  sprintf("hddm-%s", context),
  sprintf("game-%s_effect-%s_wiener", game_id, effect)
)
data <- load_data(game_id, context, effect, "{key}", {rt_min}, {rt_max})
sample_model(model, data, file = file_save, iter = 1000, warmup = 500)
