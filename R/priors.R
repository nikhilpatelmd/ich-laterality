model_parameters_function <- function(ich_aggressive) {
  formulas <- list(
    neurosurgery_minimal = neurosurgery_evac ~ ich_laterality,
    neurosurgery_canonical = neurosurgery_evac ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh,
    evd_minimal = evd ~ ich_laterality,
    evd_canonical = evd ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh,
    trach_minimal = tracheostomy ~ ich_laterality,
    trach_canonical = tracheostomy ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh,
    vent_minimal = days_mechanical_ventilation ~ ich_laterality,
    vent_canonical = days_mechanical_ventilation ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh,
    comfort_care_minimal = comfort_care_binary ~ ich_laterality,
    comfort_care_canonical = comfort_care_binary ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh,
    early_wlst_minimal = early_wlst ~ ich_laterality,
    early_wlst_canonical = early_wlst ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh,
    dnr_minimal = dnr_binary ~ ich_laterality,
    dnr_canonical = dnr_binary ~ ich_laterality + age + gcs_baseline + ich_location + ich_volume_baseline + ivh
  )

  priors <- list(
    neurosurgery_minimal = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    neurosurgery_canonical = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    evd_minimal = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    evd_canonical = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    trach_minimal = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    trach_canonical = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    vent_minimal = c(
      set_prior("normal(0, 1.5)", class = "Intercept"),
      set_prior("normal(0, 0.5)", class = "b")
    ),
    vent_canonical = c(
      set_prior("normal(0, 1.5)", class = "Intercept"),
      set_prior("normal(0, 0.5)", class = "b")
    ),
    comfort_care_minimal = c(
      set_prior("normal(1.5, 0.2)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    comfort_care_canonical = c(
      set_prior("normal(0.95, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    early_wlst_minimal = c(
      set_prior("normal(1.5, 0.2)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    early_wlst_canonical = c(
      set_prior("normal(0.95, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    dnr_minimal = c(
      set_prior("normal(1.5, 0.2)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    dnr_canonical = c(
      set_prior("normal(0.95, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    )
  )

  data <- list(
    neurosurgery_minimal = ich_aggressive,
    neurosurgery_canonical = ich_aggressive,
    evd_minimal = ich_aggressive,
    evd_canonical = ich_aggressive,
    trach_minimal = ich_aggressive,
    trach_canonical = ich_aggressive,
    vent_minimal = ich_aggressive,
    vent_canonical = ich_aggressive,
    comfort_care_minimal = ich_aggressive,
    comfort_care_canonical = ich_aggressive,
    early_wlst_minimal = ich_aggressive,
    early_wlst_canonical = ich_aggressive,
    dnr_minimal = ich_aggressive,
    dnr_canonical = ich_aggressive
  )

  family <- list(
    neurosurgery_minimal = bernoulli(link = "logit"),
    neurosurgery_canonical = bernoulli(link = "logit"),
    evd_minimal = bernoulli(link = "logit"),
    evd_canonical = bernoulli(link = "logit"),
    trach_minimal = bernoulli(link = "logit"),
    trach_canonical = bernoulli(link = "logit"),
    vent_minimal = cumulative(link = "logit"),
    vent_canonical = cumulative(link = "logit"),
    comfort_care_minimal = bernoulli(link = "logit"),
    comfort_care_canonical = bernoulli(link = "logit"),
    early_wlst_minimal = bernoulli(link = "logit"),
    early_wlst_canonical = bernoulli(link = "logit"),
    dnr_minimal = bernoulli(link = "logit"),
    dnr_canonical = bernoulli(link = "logit")
  )

  model_parameters <- tibble(
    names = names(formulas),
    formula = formulas,
    priors = priors,
    data = data,
    family = family
  )

  return(model_parameters)
}

prior_func_meta <- function(y, x) {
  prior_func <- function(formula, priors, data, family) {
    mod_prior <- brm(formula,
      data = data,
      family = family,
      prior = priors,
      sample_prior = "only"
    )
    return(mod_prior)
  }

  prior_func(
    y$formula[[x]],
    y$priors[[x]],
    y$data[[x]],
    y$family[[x]]
  )
}

posterior_func_meta <- function(y, x) {
  posterior_func <- function(formula, priors, data, family) {
    mod_prior <- brm(formula,
      data = data,
      family = family,
      prior = priors
    )

    return(mod_prior)
  }

  posterior_func(
    y$formula[[x]],
    y$priors[[x]],
    y$data[[x]],
    y$family[[x]]
  )
}
