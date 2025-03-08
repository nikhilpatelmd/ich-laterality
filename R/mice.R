f_imputed<- function(ich_aggressive) {

  variables <- c(
    "neurosurgery_evac",
    "ich_laterality",
    "ich_location",
    "age",
    "ivh",
    "gcs_baseline",
    "ich_volume_baseline",
    "study",
    "htn",
    "evd",
    'mrs_90',
    "time_symptoms_to_ed"
    )

  data <- ich_aggressive |>
    select(all_of(variables))

  imp <- mice(data, m = 100, seed = 1, print = FALSE)

  return(imp)

}

f_posterior_neutral_neurosurgery_imp <- function(dat) {
  settings <- model_setup()

  model <- brm_multiple(neurosurgery_evac ~ ich_laterality*ich_location + age + gcs_baseline + ich_volume_baseline + ivh + (1 | study),
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
    seed = settings$seed,
    control = list(adapt_delta = 0.9)
  )

  return(model)
}


# Imputed models that need to be run: 
# neurosurgery_evac, 
# evd, 
# mrs_90,
# 

f_posterior_neutral_evd_imp <- function(dat) {
  settings <- model_setup()

  model <- brm_multiple(evd ~ ich_laterality*ich_location + age + gcs_baseline + ich_volume_baseline + ivh,
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

f_posterior_neutral_mrs_90_canonical_imp <- function(dat) {
  settings <- model_setup()

  model <- brm_multiple(mrs_90 ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh + time_symptoms_to_ed ,
    family = cumulative(link = "logit"),
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
    seed = settings$seed
  )

  return(model)
}