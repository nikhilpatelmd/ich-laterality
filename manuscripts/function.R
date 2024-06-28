library(targets)
library(tidyverse)
library(withr)
library(here)
library(modelsummary)
library(marginaleffects)
library(brms)
library(tidybayes)
library(bayesplot)
library(glue)
library(ggdist)
library(patchwork)
library(tinytable)
library(gt)

neutral_mean <- 0
neutral_sd <- 0.35
neutral_lower_OR <- exp(neutral_mean - (1.96 * neutral_sd))
neutral_upper_OR <- exp(neutral_mean + (1.96 * neutral_sd))

left_mean <- -0.22
left_sd <- 0.175
left_lower_OR <- exp(left_mean - (1.96 * left_sd))
left_upper_OR <- exp(left_mean + (1.96 * left_sd))

right_mean <- 0.18
right_sd <- 0.175
right_lower_OR <- exp(right_mean - (1.96 * right_sd))
right_upper_OR <- exp(right_mean + (1.96 * right_sd))


neutral_model <- with_dir(here(), tar_read(m_prior_neutral_neurosurgery))
left_model <- with_dir(here(), tar_read(m_prior_left_neurosurgery))
right_model <- with_dir(here(), tar_read(m_prior_right_neurosurgery))
flat_model <- with_dir(here(), tar_read(m_prior_flat_neurosurgery))

f_prior_predictive_check <- function(neutral_model, left_model, right_model, flat_model) {
  models <- list(
    "Neutral Prior" = neutral_model,
    "Left Hemisphere Prior" = left_model,
    "Right Hemisphere Prior" = right_model,
    "Flat Prior" = flat_model
  )

  # gather summary variables for each model
  neutral <- models$"Neutral Prior" |>
    spread_draws(b_ich_lateralityRight) |>
    mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
    summarize(
      aor = quantile(ich_right_or, 0.5),
      lower_ci = quantile(ich_right_or, 0.025),
      upper_ci = quantile(ich_right_or, 0.975),
      or_1 = sum(ich_right_or > 1) / n(),
      rope = sum(ich_right_or > 1.05 | ich_right_or < 0.95) / n()
    )

  left <- models$"Left Hemisphere Prior" |>
    spread_draws(b_ich_lateralityRight) |>
    mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
    summarize(
      aor = quantile(ich_right_or, 0.5),
      lower_ci = quantile(ich_right_or, 0.025),
      upper_ci = quantile(ich_right_or, 0.975),
      or_1 = sum(ich_right_or > 1) / n(),
      rope = sum(ich_right_or > 1.05 | ich_right_or < 0.95) / n()
    )

  right <- models$"Right Hemisphere Prior" |>
    spread_draws(b_ich_lateralityRight) |>
    mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
    summarize(
      aor = quantile(ich_right_or, 0.5),
      lower_ci = quantile(ich_right_or, 0.025),
      upper_ci = quantile(ich_right_or, 0.975),
      or_1 = sum(ich_right_or > 1) / n(),
      rope = sum(ich_right_or > 1.05 | ich_right_or < 0.95) / n()
    )

  flat <- models$"Flat Prior" |>
    spread_draws(b_ich_lateralityRight) |>
    mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
    summarize(
      aor = quantile(ich_right_or, 0.5),
      lower_ci = quantile(ich_right_or, 0.025),
      upper_ci = quantile(ich_right_or, 0.975),
      or_1 = sum(ich_right_or > 1) / n(),
      rope = sum(ich_right_or > 1.05 | ich_right_or < 0.95) / n()
    )

  prior_summary <- rbind(neutral, left, right, flat) |>
    mutate(prior = c("Neutral", "Left Hemisphere", "Right Hemisphere", "Flat")) |>
    select(c(prior, aor, lower_ci, upper_ci, or_1, rope))

  # add prior predictions
  neutral_predictions <-
    as_tibble(avg_predictions(models$"Neutral Prior", by = "ich_laterality")) |>
    mutate("prior" = "Neutral") |>
    select(c("prior", ich_laterality, estimate)) |>
    pivot_wider(names_from = ich_laterality, values_from = estimate)

  left_predictions <-
    as_tibble(avg_predictions(models$"Left Hemisphere Prior", by = "ich_laterality")) |>
    mutate("prior" = "Left Hemisphere") |>
    select(c("prior", ich_laterality, estimate)) |>
    pivot_wider(names_from = ich_laterality, values_from = estimate)

  right_predictions <-
    as_tibble(avg_predictions(models$"Right Hemisphere Prior", by = "ich_laterality")) |>
    mutate("prior" = "Right Hemisphere") |>
    select(c("prior", ich_laterality, estimate)) |>
    pivot_wider(names_from = ich_laterality, values_from = estimate)

  flat_predictions <-
    as_tibble(avg_predictions(models$"Flat Prior", by = "ich_laterality")) |>
    mutate("prior" = "Flat") |>
    select(c("prior", ich_laterality, estimate)) |>
    pivot_wider(names_from = ich_laterality, values_from = estimate)

  prediction_summary <- rbind(neutral_predictions, left_predictions, right_predictions, flat_predictions)

  prior_prediction_summary <- left_join(prediction_summary, prior_summary, by = "prior") |>
    gt(rowname_col = "prior") |>
    tab_stubhead(label = "Type of Prior") |>
    fmt_number(columns = everything(), decimals = 2) |>
    fmt_number(columns = 6, rows = 4, decimals = 0) |>
    fmt_percent(columns = 2:3, rows = 1:4, decimals = 1) |>
    tab_header(
      title = md("**Prior Predictive Checks**")
    ) |>
    cols_label(
      Left = "Left Hemisphere",
      Right = "Right Hemisphere",
      aor = "aOR",
      lower_ci = "Lower 95% Credible Interval",
      upper_ci = "Upper 95% Credible Interval",
      or_1 = "Probability of Difference (aOR >1)",
      rope = "% of Distribution outside of ROPE"
    ) |>
    cols_width(
      1 ~ pct(50)
    )

  return(prior_prediction_summary)
}

f_prior_predictive_check(neutral_model, left_model, right_model, flat_model)

neutral_model <- with_dir(here(), tar_read(m_prior_neutral_evd))
left_model <- with_dir(here(), tar_read(m_prior_left_evd))
right_model <- with_dir(here(), tar_read(m_prior_right_evd))
flat_model <- with_dir(here(), tar_read(m_prior_flat_evd))

f_prior_predictive_check(neutral_model, left_model, right_model, flat_model)
