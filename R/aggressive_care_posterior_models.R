# Posterior models ----------------------------------------------------------

## Neurosurgery as outcome -----

f_posterior_neurosurgery_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(neurosurgery_evac ~ ich_laterality,
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    backend = "cmdstanr"
  )

  return(model)
}

f_posterior_neurosurgery_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(neurosurgery_evac ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh,
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    backend = "cmdstanr"
  )

  return(model)
}

## EVD as outcome -----

f_posterior_evd_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(evd ~ ich_laterality,
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    backend = "cmdstanr"
  )

  return(model)
}

f_posterior_evd_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(evd ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh,
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    backend = "cmdstanr"
  )

  return(model)
}

## Tracheostomy as outcome -----

f_posterior_trach_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(tracheostomy ~ ich_laterality,
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    backend = "cmdstanr"
  )

  return(model)
}

f_posterior_trach_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(tracheostomy ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh,
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    backend = "cmdstanr"
  )

  return(model)
}

## Days of Ventilation as outcome -----

f_posterior_vent_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(days_mechanical_ventilation ~ ich_laterality,
    family = cumulative(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(0, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    backend = "cmdstanr"
  )

  return(model)
}

f_posterior_vent_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(days_mechanical_ventilation ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh,
    family = cumulative(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(0, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    backend = "cmdstanr"
  )

  return(model)
}

## Comfort care as outcome -----

f_posterior_comfort_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(comfort_care_binary ~ ich_laterality,
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    backend = "cmdstanr"
  )

  return(model)
}

f_posterior_comfort_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(comfort_care_binary ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh,
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    backend = "cmdstanr"
  )

  return(model)
}

## Early Comfort care as outcome -----

f_posterior_early_comfort_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(early_wlst ~ ich_laterality,
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    backend = "cmdstanr"
  )

  return(model)
}

f_posterior_early_comfort_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(early_wlst ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh,
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    backend = "cmdstanr"
  )

  return(model)
}

## DNR as outcome -----

f_posterior_dnr_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(dnr_binary ~ ich_laterality,
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    backend = "cmdstanr"
  )

  return(model)
}

f_posterior_dnr_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(dnr_binary ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh,
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    backend = "cmdstanr"
  )

  return(model)
}
