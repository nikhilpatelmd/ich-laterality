outcome_models_list_func <- function(a, b, c, d, e, f) {
  list(
    "Modified Rankin Score" = a,
    "EuroQOL - Mobility" = b,
    "EuroQOL - Self-Care" = c,
    "EuroQOL - Usual Activities" = d,
    "EuroQOL - Pain/Discomfort" = e,
    "EuroQOL - Anxiety/Depression" = f,
  )
}

table_3_function <- function(models) {
  models <- models

  mrs <- models$"Modified Rankin Score" |>
    spread_draws(b_ich_lateralityRight) |>
    mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
    summarize(
      or = median(ich_right_or),
      lower_95_ci = quantile(ich_right_or, 0.025),
      upper_95_ci = quantile(ich_right_or, 0.975),
      or_1 = sum(ich_right_or > 1) / n(),
      or_1.1 = sum(ich_right_or > 1.1) / n(),
      or_1.2 = sum(ich_right_or > 1.2) / n(),
      rope = sum(ich_right_or < 1.05 & ich_right_or > 0.95) / n()
    ) |>
    mutate(
      or_ci = glue("{round(or, digits = 2)} ({round(lower_95_ci, digits = 2)} - {round(upper_95_ci, digits = 2)})")
    ) |>
    select(or_ci, or_1, or_1.2, rope)

  euro_anxiety <- models$"EuroQOL - Anxiety/Depression" |>
    spread_draws(b_ich_lateralityRight) |>
    mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
    summarize(
      or = median(ich_right_or),
      lower_95_ci = quantile(ich_right_or, 0.025),
      upper_95_ci = quantile(ich_right_or, 0.975),
      or_1 = sum(ich_right_or > 1) / n(),
      or_1.1 = sum(ich_right_or > 1.1) / n(),
      or_1.2 = sum(ich_right_or > 1.2) / n(),
      rope = sum(ich_right_or < 1.05 & ich_right_or > 0.95) / n()
    ) |>
    mutate(
      or_ci = glue("{round(or, digits = 2)} ({round(lower_95_ci, digits = 2)} - {round(upper_95_ci, digits = 2)})")
    ) |>
    select(or_ci, or_1, or_1.2, rope)

  euro_mobility <- models$"EuroQOL - Mobility" |>
    spread_draws(b_ich_lateralityRight) |>
    mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
    summarize(
      or = median(ich_right_or),
      lower_95_ci = quantile(ich_right_or, 0.025),
      upper_95_ci = quantile(ich_right_or, 0.975),
      or_1 = sum(ich_right_or > 1) / n(),
      or_1.1 = sum(ich_right_or > 1.1) / n(),
      or_1.2 = sum(ich_right_or > 1.2) / n(),
      rope = sum(ich_right_or < 1.05 & ich_right_or > 0.95) / n()
    ) |>
    mutate(
      or_ci = glue("{round(or, digits = 2)} ({round(lower_95_ci, digits = 2)} - {round(upper_95_ci, digits = 2)})")
    ) |>
    select(or_ci, or_1, or_1.2, rope)

  euro_pain <- models$"EuroQOL - Pain/Discomfort" |>
    spread_draws(b_ich_lateralityRight) |>
    mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
    summarize(
      or = median(ich_right_or),
      lower_95_ci = quantile(ich_right_or, 0.025),
      upper_95_ci = quantile(ich_right_or, 0.975),
      or_1 = sum(ich_right_or > 1) / n(),
      or_1.1 = sum(ich_right_or > 1.1) / n(),
      or_1.2 = sum(ich_right_or > 1.2) / n(),
      rope = sum(ich_right_or < 1.05 & ich_right_or > 0.95) / n()
    ) |>
    mutate(
      or_ci = glue("{round(or, digits = 2)} ({round(lower_95_ci, digits = 2)} - {round(upper_95_ci, digits = 2)})")
    ) |>
    select(or_ci, or_1, or_1.2, rope)

  euro_selfcare <- models$"EuroQOL - Self-Care" |>
    spread_draws(b_ich_lateralityRight) |>
    mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
    summarize(
      or = median(ich_right_or),
      lower_95_ci = quantile(ich_right_or, 0.025),
      upper_95_ci = quantile(ich_right_or, 0.975),
      or_1 = sum(ich_right_or > 1) / n(),
      or_1.1 = sum(ich_right_or > 1.1) / n(),
      or_1.2 = sum(ich_right_or > 1.2) / n(),
      rope = sum(ich_right_or < 1.05 & ich_right_or > 0.95) / n()
    ) |>
    mutate(
      or_ci = glue("{round(or, digits = 2)} ({round(lower_95_ci, digits = 2)} - {round(upper_95_ci, digits = 2)})")
    ) |>
    select(or_ci, or_1, or_1.2, rope)

  euro_usual <- models$"EuroQOL - Usual Activities" |>
    spread_draws(b_ich_lateralityRight) |>
    mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
    summarize(
      or = median(ich_right_or),
      lower_95_ci = quantile(ich_right_or, 0.025),
      upper_95_ci = quantile(ich_right_or, 0.975),
      or_1 = sum(ich_right_or > 1) / n(),
      or_1.1 = sum(ich_right_or > 1.1) / n(),
      or_1.2 = sum(ich_right_or > 1.2) / n(),
      rope = sum(ich_right_or < 1.05 & ich_right_or > 0.95) / n()
    ) |>
    mutate(
      or_ci = glue("{round(or, digits = 2)} ({round(lower_95_ci, digits = 2)} - {round(upper_95_ci, digits = 2)})")
    ) |>
    select(or_ci, or_1, or_1.2, rope)

  total_tibble <- bind_rows(
    "Modified Rankin Score" = mrs,
    "EuroQOL - Mobility" = euro_mobility,
    "EuroQOL - Self-Care" = euro_selfcare,
    "EuroQOL - Usual Activities" = euro_usual,
    "EuroQOL - Pain/Discomfort" = euro_pain,
    "EuroQOL - Anxiety/Depression" = euro_anxiety,
    .id = "Outcome"
  )
}
