vent_mod <- brm(
  days_mechanical_ventilation ~ ich_laterality,
  data = ich_aggressive,
  family = cumulative(link = "logit"),
  prior = c(set_prior("normal(0, 1.5)", class = "Intercept"))
)
