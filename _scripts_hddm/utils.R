load_data <- function(game_id,
                      context = c("retest", "camp"),
                      effect = c("simple", "switch", "cong"),
                      key = NULL,
                      rt_min = NA,
                      rt_max = NA) {
  context <- match.arg(context)
  effect <- match.arg(effect)
  rt_min <- coalesce(rt_min, 0)
  rt_max <- coalesce(rt_max, Inf)
  projects <- targets::tar_config_yaml()
  switch(context,
    retest = targets::tar_read_raw(
      paste0("data_valid_", game_id),
      store = projects$prepare_source_data_retest$store
    ) |>
      filter(n() == 2, .by = user_id) |>
      mutate(
        ocassion = if_else(row_number(game_time) == 1, "test", "retest"),
        .by = user_id
      ) |>
      unnest(raw_parsed),
    camp = targets::tar_read_raw(
      paste0("data_valid_", game_id),
      store = projects$prepare_source_data$store
    ) |>
      unnest(raw_parsed)
  ) |>
    mutate(
      rt = RT / 1000,
      # unify levels
      type = if (effect == "switch") {
        factor(tolower(.data[[key]]), c("repeat", "switch"))
      } else if (effect == "cong") {
        factor(str_sub(tolower(.data[[key]]), end = 3), c("con", "inc"))
      }
    ) |>
    filter(if_all(any_of("type"), \(x) !is.na(x)), rt > rt_min, rt < rt_max) |>
    select(c(
      "user_id",
      if (context == "retest") "ocassion",
      if (effect != "simple") "type",
      acc = "ACC",
      "rt"
    ))
}

sample_model <- function(model, data, chains = 4, ...) {
  inits <- with(
    standata(update(model, newdata = data, chains = 0)),
    replicate(
      chains,
      list(
        b = as.array(rnorm(K)),
        b_bs = as.array(runif(K_bs, 1, 2)),
        b_ndt = as.array(runif(K_ndt, 0.05, 0.1)),
        sd_1 = as.array(runif(M_1, 0.5, 1)),
        z_1 = matrix(rnorm(M_1 * N_1, 0, 0.01), M_1, N_1),
        L_1 = diag(M_1)
      ),
      simplify = FALSE
    )
  )
  update(
    model,
    newdata = data,
    init = inits,
    chains = chains,
    cores = chains,
    ...
  )
}
