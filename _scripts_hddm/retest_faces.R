suppressPackageStartupMessages({
  library(tidyverse)
  library(brms)
})
requireNamespace("bit64", quietly = TRUE)
projects <- targets::tar_config_yaml()
game_id <- "224376556507589"
file_save <- str_glue("data-raw/hddm-retest/game-{game_id}_type-switch_wiener")
cat("Processing: ", data.iquizoo::match_info(game_id, "game_name"), "\n\n")
data <- targets::tar_read_raw(
  paste0("data_valid_", game_id),
  store = projects$prepare_source_data_retest$store
) |>
  filter(n() == 2, .by = user_id) |>
  mutate(
    ocassion = if_else(row_number(game_time) == 1, "test", "retest"),
    .by = user_id
  ) |>
  unnest(raw_parsed) |>
  mutate(
    RT = RT / 1000,
    # unify levels
    TaskType = case_match(
      TaskType,
      "Switch" ~ "switch",
      "Repeat" ~ "repeat"
    )
  ) |>
  filter(RT > 0.25 & RT < 10, !is.na(TaskType)) |>
  select(user_id, ocassion, type = TaskType, acc = ACC, rt = RT)
model <- readRDS("data/model_diff_retest.rds")

if (interactive()) {
  View(standata(update(model, newdata = data, chains = 0)))
}

initfun <- function() {
  K <- 4 # 2 by 2
  K_bs <- 2
  K_ndt <- 2
  M_1 <- K + K_bs + K_ndt
  N_1 <- 388 # number of subjects
  list(
    b = as.array(rnorm(K)),
    b_bs = as.array(runif(K_bs, 1, 2)),
    b_ndt = as.array(runif(K_ndt, 0.05, 0.1)),
    sd_1 = as.array(runif(M_1, 0.5, 1)),
    z_1 = matrix(rnorm(M_1 * N_1, 0, 0.01), M_1, N_1),
    L_1 = diag(M_1)
  )
}
fit_wiener <- update(
  model,
  newdata = data,
  init = initfun,
  file = file_save,
  iter = 1000,
  warmup = 500,
  chains = 4,
  cores = 4,
  control = list(max_treedepth = 15, adapt_delta = 0.9)
)
