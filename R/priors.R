model_parameters_function <- function(ich_aggressive) {
  formulas <- list(
    neurosurgery_minimal = neurosurgery_evac ~ ich_laterality,
    neurosurgery_canonical = neurosurgery_evac ~ ich_laterality + age + htn + stroke + gcs_baseline + ich_location + ich_volume_baseline + ivh,
    evd_minimal = evd ~ ich_laterality,
    evd_canonical = evd ~ ich_laterality + age + htn + stroke + gcs_baseline + ich_location + ich_volume_baseline + ivh
  )

  priors <- list(
    neurosurgery_minimal = c(
      set_prior("normal(-2.2, 0.1)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    neurosurgery_canonical = c(
      set_prior("normal(-2.2, 1)", class = "Intercept"),
      set_prior("normal(0, 1.5)", class = "b")
    ),
    evd_minimal = c(
      set_prior("normal(-2.2, 0.7)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),
    evd_canonical = c(
      set_prior("normal(-2.2, 1)", class = "Intercept"),
      set_prior("normal(0, 1.5)", class = "b")
    )
  )

  data <- list(
    neurosurgery_minimal = ich_aggressive,
    neurosurgery_canonical = ich_aggressive,
    evd_minimal = ich_aggressive,
    evd_canonical = ich_aggressive
  )

  family <- list(
    neurosurgery_minimal = bernoulli(link = "logit"),
    neurosurgery_canonical = bernoulli(link = "logit"),
    evd_minimal = bernoulli(link = "logit"),
    evd_canonical = bernoulli(link = "logit")
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
