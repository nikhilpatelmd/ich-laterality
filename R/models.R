# neurosurgical intervention
neurosurgery_evac_func <- function(x) {
  BAYES_SEED <- 07204

  options(
    mc.cores = 4,
    brms.backend = "cmdstanr"
  )

  priors <- c(
    set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
    set_prior("normal(0, 1)", class = "b")
  )

  brm(
    neurosurgery_evac ~ ich_laterality + (1 | study),
    data = x,
    family = bernoulli(link = "logit"),
    prior = priors,
    init = 0,
    chains = 4, iter = 4000, seed = BAYES_SEED
  )
}

neurosurgery_evac_func_canon <- function(x) {
  BAYES_SEED <- 07204

  options(
    mc.cores = 4,
    brms.backend = "cmdstanr"
  )

  priors <- c(
    set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
    set_prior("normal(0, 1)", class = "b")
  )

  brm(
    neurosurgery_evac ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + stroke + (1 | study),
    data = x,
    family = bernoulli(link = "logit"),
    prior = priors,
    init = 0,
    chains = 4, iter = 4000, seed = BAYES_SEED
  )
}
