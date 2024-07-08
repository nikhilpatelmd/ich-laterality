# Posterior models ----------------------------------------------------------

## Neurosurgery as outcome -----

f_posterior_neutral_neurosurgery <- function(dat) {
  settings <- model_setup()

  model <- brm(neurosurgery_evac ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-7, 0.35)", class = "Intercept"),
      set_prior("normal(0, 0.5)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_posterior_left_neurosurgery <- function(dat) {
  settings <- model_setup()

  model <- brm(neurosurgery_evac ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-7, 0.5)", class = "Intercept"),
      set_prior("normal(-0.22, 0.175)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_posterior_right_neurosurgery <- function(dat) {
  settings <- model_setup()

  model <- brm(neurosurgery_evac ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-7, 0.5)", class = "Intercept"),
      set_prior("normal(0.18, 0.175)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_posterior_flat_neurosurgery <- function(dat) {
  settings <- model_setup()

  model <- brm(neurosurgery_evac ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(0, 1)", class = "Intercept"),
      set_prior("normal(0, 5)", class = "b")
    ),
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

f_posterior_neutral_evd <- function(dat) {
  settings <- model_setup()

  model <- brm(evd ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh,
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(0, 0.5)", class = "Intercept"),
      set_prior("normal(0, 0.35)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_posterior_left_evd <- function(dat) {
  settings <- model_setup()

  model <- brm(evd ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(0, 0.5)", class = "Intercept"),
      set_prior("normal(-0.22, 0.175)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_posterior_right_evd <- function(dat) {
  settings <- model_setup()

  model <- brm(evd ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(0, 0.5)", class = "Intercept"),
      set_prior("normal(0.18, 0.175)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_posterior_flat_evd <- function(dat) {
  settings <- model_setup()

  model <- brm(evd ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(0, 1)", class = "Intercept"),
      set_prior("normal(0, 5)", class = "b")
    ),
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

f_posterior_neutral_tracheostomy <- function(dat) {
  settings <- model_setup()

  model <- brm(tracheostomy ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-15, 0.5)", class = "Intercept"),
      set_prior("normal(0, 0.35)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_posterior_left_tracheostomy <- function(dat) {
  settings <- model_setup()

  model <- brm(tracheostomy ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-15, 0.5)", class = "Intercept"),
      set_prior("normal(-0.22, 0.175)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_posterior_right_tracheostomy <- function(dat) {
  settings <- model_setup()

  model <- brm(tracheostomy ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-15, 0.5)", class = "Intercept"),
      set_prior("normal(0.18, 0.175)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_posterior_flat_tracheostomy <- function(dat) {
  settings <- model_setup()

  model <- brm(tracheostomy ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(0, 1)", class = "Intercept"),
      set_prior("normal(0, 5)", class = "b")
    ),
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

f_posterior_neutral_days_mechanical_ventilation <- function(dat) {
  settings <- model_setup()

  model <- brm(days_mechanical_ventilation ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh,
    family = cumulative(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-5, 0.5)", class = "Intercept"),
      set_prior("normal(0, 0.35)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_posterior_left_days_mechanical_ventilation <- function(dat) {
  settings <- model_setup()

  model <- brm(days_mechanical_ventilation ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh,
    family = cumulative(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-5, 0.5)", class = "Intercept"),
      set_prior("normal(-0.22, 0.175)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_posterior_right_days_mechanical_ventilation <- function(dat) {
  settings <- model_setup()

  model <- brm(days_mechanical_ventilation ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh,
    family = cumulative(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-5, 0.5)", class = "Intercept"),
      set_prior("normal(0.18, 0.175)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_posterior_flat_days_mechanical_ventilation <- function(dat) {
  settings <- model_setup()

  model <- brm(days_mechanical_ventilation ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh,
    family = cumulative(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-5, 0.5)", class = "Intercept"),
      set_prior("normal(0, 5)", class = "b")
    ),
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

f_posterior_neutral_comfort <- function(dat) {
  settings <- model_setup()

  model <- brm(comfort_care_binary ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-5, 0.5)", class = "Intercept"),
      set_prior("normal(0, 0.35)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_posterior_left_comfort <- function(dat) {
  settings <- model_setup()

  model <- brm(comfort_care_binary ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-5, 0.5)", class = "Intercept"),
      set_prior("normal(-0.22, 0.175)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_posterior_right_comfort <- function(dat) {
  settings <- model_setup()

  model <- brm(comfort_care_binary ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-5, 0.5)", class = "Intercept"),
      set_prior("normal(0.18, 0.175)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_posterior_flat_comfort <- function(dat) {
  settings <- model_setup()

  model <- brm(comfort_care_binary ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(0, 1)", class = "Intercept"),
      set_prior("normal(0, 5)", class = "b")
    ),
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

f_posterior_neutral_early_wlst <- function(dat) {
  settings <- model_setup()

  model <- brm(early_wlst ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-5, 0.5)", class = "Intercept"),
      set_prior("normal(0, 0.35)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_posterior_left_early_wlst <- function(dat) {
  settings <- model_setup()

  model <- brm(early_wlst ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-5, 0.5)", class = "Intercept"),
      set_prior("normal(-0.22, 0.175)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_posterior_right_early_wlst <- function(dat) {
  settings <- model_setup()

  model <- brm(early_wlst ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-5, 0.5)", class = "Intercept"),
      set_prior("normal(0.18, 0.175)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_posterior_flat_early_wlst <- function(dat) {
  settings <- model_setup()

  model <- brm(early_wlst ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(0, 1)", class = "Intercept"),
      set_prior("normal(0, 5)", class = "b")
    ),
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

f_posterior_neutral_dnr_binary <- function(dat) {
  settings <- model_setup()

  model <- brm(dnr_binary ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-5, 0.5)", class = "Intercept"),
      set_prior("normal(0, 0.35)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_posterior_left_dnr_binary <- function(dat) {
  settings <- model_setup()

  model <- brm(dnr_binary ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-5, 0.5)", class = "Intercept"),
      set_prior("normal(-0.22, 0.175)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_posterior_right_dnr_binary <- function(dat) {
  settings <- model_setup()

  model <- brm(dnr_binary ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-5, 0.5)", class = "Intercept"),
      set_prior("normal(0.18, 0.175)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_posterior_flat_dnr_binary <- function(dat) {
  settings <- model_setup()

  model <- brm(dnr_binary ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(0, 1)", class = "Intercept"),
      set_prior("normal(0, 5)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}
