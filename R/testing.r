priors <- c(
  set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
  set_prior("normal(0, 1)", class = "b")
)

model <- brm(
  neurosurgery_evac ~ ich_laterality + (1 | study),
  data = non_surgery_trials,
  family = bernoulli(link = "logit"),
  prior = priors,
  init = 0,
  chains = 4, iter = 4000, seed = BAYES_SEED
)

model
