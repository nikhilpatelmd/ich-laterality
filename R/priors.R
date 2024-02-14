# Variables to include: age, gcs_baseline, hypertension, ich_location, ich_volume, ivh, stroke
# Outcomes: neurosurgery, evd, days_mechanical_ventilation, tracheostomy, peg, comfort_care

# neurosurgical intervention
neurosurgery_evac_min <- function(x) {
  BAYES_SEED <- 07204

  priors <- c(
    set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
    set_prior("normal(0, 1)", class = "b")
  )

  brm(
    neurosurgery_evac ~ ich_laterality,
    data = non_surgery_trials,
    family = bernoulli(link = "logit"),
    prior = priors,
    init = 0,
    chains = 4, iter = 4000, seed = BAYES_SEED,
    backend = "cmdstanr"
  )
}
