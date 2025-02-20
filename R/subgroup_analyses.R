tar_load(m_posterior_neutral_neurosurgery)
tar_load(ich_aggressive)

# Fit the Bayesian logistic regression model with interaction
model_with_interaction <- brm(
  neurosurgery_evac ~ ich_laterality * ich_location + age + gcs_baseline + ich_volume_baseline + ivh + (1 | study),
  data = ich_aggressive,
  family = bernoulli(link = "logit"),
  prior = c(
    set_prior("normal(0, 10)", class = "Intercept"),
    set_prior("normal(0, 2.5)", class = "b"),
    set_prior("exponential(1)", class = "sd")
  ),
  chains = 4,
  backend = "cmdstanr",
  iter = 4000,
  control = list(adapt_delta = 0.95)
)

# Calculate predicted probabilities
pred <- predictions(
  m_posterior_neutral_neurosurgery,
  by = c("ich_laterality", "ich_location"),
  type = "response"
) |>
  get_draws()

# Calculate odds for each draw
pred <- pred |>
  mutate(odds = estimate / (1 - estimate))

# Calculate odds ratios for each draw
pred <- pred |>
  group_by(ich_location, drawid)


bob <- m_posterior_neutral_neurosurgery |>
  avg_comparisons(
    variables = "ich_laterality",
  )

  ame_or <- avg_comparisons(
    model_with_interaction,
    variables = "ich_laterality",  # AME for ich_laterality
    by = "ich_location",           # Stratified by ich_location
    type = "link",
    transform = "exp"              # Exponentiate to get odds ratio
  )