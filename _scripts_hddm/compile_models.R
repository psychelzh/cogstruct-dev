library(tidyverse)
library(brms)
family <- wiener(
  link_bs = "identity",
  link_ndt = "identity"
)
prior <- c(
  set_prior("cauchy(0, 5)", class = "b"),
  set_prior("gamma(3, .5)", class = "b", dpar = "bs", lb = 0),
  set_prior("gamma(1, .5)", class = "b", dpar = "ndt", lb = 0)
)
# retest models
## congruency effect/switch cost
brm(
  bf(
    rt | dec(acc) ~ 0 + ocassion:type + (0 + ocassion:type | p | user_id),
    bs ~ 0 + ocassion + (0 + ocassion | p | user_id),
    ndt ~ 0 + ocassion + (0 + ocassion | p | user_id),
    bias = 0.5
  ),
  withr::with_seed(
    1,
    expand_grid(
      user_id = 0,
      ocassion = c("test", "retest"),
      type = c("inc", "con") # could be changed to switch related levels
    ) |>
      mutate(
        rt = rexp(n()),
        acc = sample(c(0, 1), n(), replace = TRUE)
      )
  ),
  family = family,
  prior = prior,
  file = "data/model_diff_retest",
  chains = 0
)
## simple intercept models
brm(
  bf(
    rt | dec(acc) ~ 0 + ocassion + (0 + ocassion | p | user_id),
    bs ~ 0 + ocassion + (0 + ocassion | p | user_id),
    ndt ~ 0 + ocassion + (0 + ocassion | p | user_id),
    bias = 0.5
  ),
  withr::with_seed(
    1,
    expand_grid(
      user_id = 0,
      ocassion = c("test", "retest")
    ) |>
      mutate(
        rt = rexp(n()),
        acc = sample(c(0, 1), n(), replace = TRUE)
      )
  ),
  family = family,
  prior = prior,
  file = "data/model_simple_retest",
  chains = 0
)

# one-time models
## congruency effect/switch cost
brm(
  bf(
    rt | dec(acc) ~ 0 + type + (0 + type | p | user_id),
    bs ~ 0 + Intercept + (1 | p | user_id),
    ndt ~ 0 + Intercept + (1 | p | user_id),
    bias = 0.5
  ),
  withr::with_seed(
    1,
    expand_grid(
      user_id = 0,
      type = c("inc", "con") # could be changed to switch related levels
    ) |>
      mutate(
        rt = rexp(n()),
        acc = sample(c(0, 1), n(), replace = TRUE)
      )
  ),
  family = family,
  prior = prior,
  file = "data/model_diff_camp",
  chains = 0
)
## simple intercept models
brm(
  bf(
    rt | dec(acc) ~ 0 + Intercept + (1 | p | user_id),
    bs ~ 0 + Intercept + (1 | p | user_id),
    ndt ~ 0 + Intercept + (1 | p | user_id),
    bias = 0.5
  ),
  withr::with_seed(
    1,
    tibble(user_id = 0) |>
      mutate(
        rt = rexp(n()),
        acc = sample(c(0, 1), n(), replace = TRUE)
      )
  ),
  family = family,
  prior = prior,
  file = "data/model_simple_camp",
  chains = 0
)
