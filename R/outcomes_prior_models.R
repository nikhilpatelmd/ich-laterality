# Prior models ----------------------------------------------------------

## mrs90 as outcome -----

f_prior_mrs_90_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(mrs_90 ~ ich_laterality,
    family = cumulative(link = "logit"),
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

f_prior_mrs_90_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(mrs_90 ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + time_symptoms_to_ed,
    family = cumulative(link = "logit"),
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

## mrs180 as outcome -----

f_prior_mrs_180_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(mrs_180 ~ ich_laterality,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

f_prior_mrs_180_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(mrs_180 ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + time_symptoms_to_ed,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

## mrs365 as outcome -----

f_prior_mrs_365_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(mrs_365 ~ ich_laterality,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

f_prior_mrs_365_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(mrs_365 ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + time_symptoms_to_ed,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

## euroQOL mobility 90 as outcome -----

f_prior_euro_mobility_90_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_mobility_90 ~ ich_laterality,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

f_prior_euro_mobility_90_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_mobility_90 ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + time_symptoms_to_ed,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

## euroQOL mobility 180 as outcome -----

f_prior_euro_mobility_180_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_mobility_180 ~ ich_laterality,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

f_prior_euro_mobility_180_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_mobility_180 ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + time_symptoms_to_ed,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

## euroQOL mobility 365 as outcome -----

f_prior_euro_mobility_365_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_mobility_365 ~ ich_laterality,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

f_prior_euro_mobility_365_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_mobility_365 ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + time_symptoms_to_ed,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

## euroQOL self-care 90 as outcome -----

f_prior_euro_selfcare_90_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_selfcare_90 ~ ich_laterality,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

f_prior_euro_selfcare_90_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_selfcare_90 ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + time_symptoms_to_ed,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

## euroQOL self-care 180 as outcome -----

f_prior_euro_selfcare_180_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_selfcare_180 ~ ich_laterality,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

f_prior_euro_selfcare_180_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_selfcare_180 ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + time_symptoms_to_ed,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

## euroQOL self-care 365 as outcome -----

f_prior_euro_selfcare_365_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_selfcare_365 ~ ich_laterality,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

f_prior_euro_selfcare_365_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_selfcare_365 ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + time_symptoms_to_ed,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

## euroQOL usual activities 90 as outcome -----

f_prior_euro_usual_90_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_usual_90 ~ ich_laterality,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

f_prior_euro_usual_90_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_usual_90 ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + time_symptoms_to_ed,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

## euroQOL usual 180 as outcome -----

f_prior_euro_usual_180_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_usual_180 ~ ich_laterality,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

f_prior_euro_usual_180_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_usual_180 ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + time_symptoms_to_ed,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

## euroQOL usual 365 as outcome -----

f_prior_euro_usual_365_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_usual_365 ~ ich_laterality,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

f_prior_euro_usual_365_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_usual_365 ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + time_symptoms_to_ed,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

## euroQOL pain 90 as outcome -----

f_prior_euro_pain_90_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_pain_90 ~ ich_laterality,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

f_prior_euro_pain_90_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_pain_90 ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + time_symptoms_to_ed,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

## euroQOL pain 180 as outcome -----

f_prior_euro_pain_180_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_pain_180 ~ ich_laterality,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

f_prior_euro_pain_180_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_pain_180 ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + time_symptoms_to_ed,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

## euroQOL pain 365 as outcome -----

f_prior_euro_pain_365_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_pain_365 ~ ich_laterality,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

f_prior_euro_pain_365_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_pain_365 ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + time_symptoms_to_ed,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

## euroQOL anxiety 90 as outcome -----

f_prior_euro_anxiety_90_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_anxiety_90 ~ ich_laterality,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

f_prior_euro_anxiety_90_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_anxiety_90 ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + time_symptoms_to_ed,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

## euroQOL anxiety 180 as outcome -----

f_prior_euro_anxiety_180_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_anxiety_180 ~ ich_laterality,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

f_prior_euro_anxiety_180_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_anxiety_180 ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + time_symptoms_to_ed,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

## euroQOL anxiety 365 as outcome -----

f_prior_euro_anxiety_365_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_anxiety_365 ~ ich_laterality,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

f_prior_euro_anxiety_365_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_anxiety_365 ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + time_symptoms_to_ed,
    family = cumulative(link = "logit"),
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

## euroQOL vas 90 as outcome -----

f_prior_euro_vas_90_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_vas_90 ~ ich_laterality,
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

f_prior_euro_vas_90_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_vas_90 ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + time_symptoms_to_ed,
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

## euroQOL vas 180 as outcome -----

f_prior_euro_vas_180_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_vas_180 ~ ich_laterality,
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

f_prior_euro_vas_180_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_vas_180 ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + time_symptoms_to_ed,
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

## euroQOL vas 365 as outcome -----

f_prior_euro_vas_365_minimal <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_vas_180 ~ ich_laterality,
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}

f_prior_euro_vas_365_canonical <- function(dat) {
  settings <- model_setup()

  model <- brm(euro_vas_180 ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + time_symptoms_to_ed,
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
    seed = settings$seed,
    backend = "rstan"
  )

  return(model)
}
