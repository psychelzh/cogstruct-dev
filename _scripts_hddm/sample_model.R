sample_model <- function(data, context, effect, chains = 4, ...) {
  model <- bf_model(data, context, effect)
  sample_impl(data, model, chains, ...)
}

sample_model2 <- function(data, context, effect, chains = 4, ...) {
  # only works for "retest2" context
  stopifnot(context == "retest2")
  data <- deframe(nest(data, .by = ocassion))
  model <- bf_model(data[[1]], "camp", effect)
  lapply(data, sample_impl, model = model, chains = chains, ...)
}

bf_model <- function(data, context, effect) {
  terms <- switch(effect,
    cong = ,
    switch = ,
    nback = ,
    anti = ,
    alert = ,
    orient = switch(context,
      retest = "ocassion:type",
      camp = "type"
    ),
    comp = switch(context,
      retest = "ocassion:stimtype:tasktype",
      camp = "stimtype:tasktype"
    ),
    simple = switch(context,
      retest = "ocassion",
      camp = "Intercept"
    )
  )
  rhs <- str_glue("0 + {terms} + (0 + {terms} | p | user_id)")
  brm(
    bf(
      str_glue("rt | dec(acc) ~ {rhs}"),
      str_glue("bs ~ {rhs}"),
      str_glue("ndt ~ {rhs}"),
      bias = 0.5
    ),
    data = data,
    family = wiener(
      link_bs = "identity",
      link_ndt = "identity"
    ),
    prior = c(
      set_prior("cauchy(0, 5)", class = "b"),
      set_prior("gamma(3, .5)", class = "b", dpar = "bs", lb = 0),
      set_prior("gamma(1, .5)", class = "b", dpar = "ndt", lb = 0)
    ),
    chains = 0
  )
}

sample_impl <- function(data, model, chains = 4, ...) {
  update(
    model,
    newdata = data,
    init = with(
      standata(model),
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
    ),
    chains = chains,
    cores = chains,
    ...
  )
}
