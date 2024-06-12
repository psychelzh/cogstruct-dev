suppressPackageStartupMessages({{
  library(tidyverse)
  library(brms)
}})
source("_scripts_hddm/utils.R")
requireNamespace("bit64", quietly = TRUE)
context <- "{context}"
game_id <- "{game_id}"
effect <- "{effect}"
model_name <- switch(effect,
  cong = ,
  switch = ,
  nback = ,
  anti = ,
  alert = ,
  orient = "diff",
  effect
)
model <- readRDS(
  fs::path(
    "data", "hddm-models",
    sprintf("model_%s_%s.rds", model_name, context)
  )
)
cat("Context: ", context, "\n")
cat("Processing: ", data.iquizoo::match_info(game_id, "game_name"), "\n")
cat("Effect: ", effect, "\n")
file_save <- fs::path(
  "data-raw",
  sprintf("hddm-%s", context),
  sprintf("game-%s_effect-%s_wiener", game_id, effect)
)
data <- load_data(context, game_id, effect, "{key}", {rt_min}, {rt_max})
sample_model(model, data, file = file_save, iter = 1000, warmup = 500)
