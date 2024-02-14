# Neurosurgical Intervention: Minimal
neurosurgery_min_prior_f <- function(x) {
  priors <- c(
    set_prior("normal(-2.19, 0.7)", class = "Intercept"),
    set_prior("normal(0, 1)", class = "b")
  )

  mod_prior <- brm(
    neurosurgery_evac ~ ich_laterality,
    data = x,
    family = bernoulli(link = "logit"),
    prior = priors,
    sample_prior = "only"
  )

  return(mod_prior)
}
