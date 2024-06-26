# Settings ----------------------------------------------------------------

model_setup <- function() {
  # Settings
  CHAINS <- 4
  CORES <- 3
  THREADS <- threading(2)
  ITER <- 2000
  WARMUP <- 1000
  BAYES_SEED <- 908

  return(list(
    chains = CHAINS, cores = CORES, threads = THREADS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED
  ))
}

# Prior models ----------------------------------------------------------

## Neurosurgery as outcome -----

f_prior_neutral_neurosurgery <- function(dat) {
  settings <- model_setup()

  model <- brm(neurosurgery_evac ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,e
    prior = c(
      set_prior("normal(0, 0.35)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    sample_prior = "only",
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_prior_informative_neurosurgery <- function(dat) {
  settings <- model_setup()

  model <- brm(neurosurgery_evac ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-20, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    sample_prior = "only",
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

## EVD as outcome -----

f_prior_evd_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(evd ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh,
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    sample_prior = "only",
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

## Tracheostomy as outcome -----

f_prior_trach_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(tracheostomy ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh,
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    sample_prior = "only",
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

## Days of Ventilation as outcome -----

f_prior_vent_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(days_mechanical_ventilation ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh,
    family = cumulative(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(0, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    sample_prior = "only",
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

## Comfort care as outcome -----

f_prior_comfort_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(comfort_care_binary ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh,
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    sample_prior = "only",
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

## Early Comfort care as outcome -----

f_prior_early_comfort_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(early_wlst ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh,
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    sample_prior = "only",
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

## DNR as outcome -----

f_prior_dnr_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(dnr_binary ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh,
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    sample_prior = "only",
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}
