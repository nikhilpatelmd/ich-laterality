

mod <- avg_predictions(m_posterior_mrs_90_canonical, by = "ich_laterality")

data <- mod |>
  select(group, ich_laterality, estimate) |>
  as_tibble() |>
  mutate(estimate = estimate * 100)

mrs_90 <- m_posterior_mrs_90_canonical |>
  spread_draws(b_ich_lateralityRight) |>
  mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
  summarize(
    or_1 = sum(ich_right_or > 1) / n(),
    or_1.1 = sum(ich_right_or > 1.1) / n(),
    or_1.2 = sum(ich_right_or > 1.2) / n(),
    rope = sum(ich_right_or < 1.05 & ich_right_or > 0.95) / n()
  )

data <- mod |>
  select(group, ich_laterality, estimate) |>
  as_tibble() |>
  mutate(estimate = estimate * 100)

mrs_180 <- m_posterior_mrs_90_canonical |>
  spread_draws(b_ich_lateralityRight) |>
  mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
  summarize(
    or_1 = sum(ich_right_or > 1) / n(),
    or_1.1 = sum(ich_right_or > 1.1) / n(),
    or_1.2 = sum(ich_right_or > 1.2) / n(),
    rope = sum(ich_right_or < 1.05 & ich_right_or > 0.95) / n()
  )

data <- mod |>
  select(group, ich_laterality, estimate) |>
  as_tibble() |>
  mutate(estimate = estimate * 100)

mrs_365 <- m_posterior_mrs_90_canonical |>
  spread_draws(b_ich_lateralityRight) |>
  mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
  summarize(
    or_1 = sum(ich_right_or > 1) / n(),
    or_1.1 = sum(ich_right_or > 1.1) / n(),
    or_1.2 = sum(ich_right_or > 1.2) / n(),
    rope = sum(ich_right_or < 1.05 & ich_right_or > 0.95) / n()
  )

euro_anxiety <- m_posterior_euro_anxiety_90_canonical |>
  spread_draws(b_ich_lateralityRight) |>
  mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
  summarize(
    or_1 = sum(ich_right_or > 1) / n(),
    or_1.1 = sum(ich_right_or > 1.1) / n(),
    or_1.2 = sum(ich_right_or > 1.2) / n(),
    rope = sum(ich_right_or < 1.05 & ich_right_or > 0.95) / n()
  )


euro_mobility <- m_posterior_euro_mobility_90_canonical |>
  spread_draws(b_ich_lateralityRight) |>
  mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
  summarize(
    or_1 = sum(ich_right_or > 1) / n(),
    or_1.1 = sum(ich_right_or > 1.1) / n(),
    or_1.2 = sum(ich_right_or > 1.2) / n(),
    rope = sum(ich_right_or < 1.05 & ich_right_or > 0.95) / n()
  )

euro_mobility

euro_pain <- m_posterior_euro_pain_90_canonical |>
  spread_draws(b_ich_lateralityRight) |>
  mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
  summarize(
    or_1 = sum(ich_right_or > 1) / n(),
    or_1.1 = sum(ich_right_or > 1.1) / n(),
    or_1.2 = sum(ich_right_or > 1.2) / n(),
    rope = sum(ich_right_or < 1.05 & ich_right_or > 0.95) / n()
  )

euro_selfcare <- m_posterior_euro_selfcare_90_canonical |>
  spread_draws(b_ich_lateralityRight) |>
  mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
  summarize(
    or_1 = sum(ich_right_or > 1) / n(),
    or_1.1 = sum(ich_right_or > 1.1) / n(),
    or_1.2 = sum(ich_right_or > 1.2) / n(),
    rope = sum(ich_right_or < 1.05 & ich_right_or > 0.95) / n()
  )

euro_usual <- m_posterior_euro_usual_90_canonical |>
  spread_draws(b_ich_lateralityRight) |>
  mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
  summarize(
    or_1 = sum(ich_right_or > 1) / n(),
    or_1.1 = sum(ich_right_or > 1.1) / n(),
    or_1.2 = sum(ich_right_or > 1.2) / n(),
    rope = sum(ich_right_or < 1.05 & ich_right_or > 0.95) / n()
  )