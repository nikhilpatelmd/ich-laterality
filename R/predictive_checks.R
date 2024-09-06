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
      or_1 = sum(ich_right_or < 1) / n(),
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
    ) |>
    tab_footnote(
      footnote = "For left hemisphere prior, the p(aOR) < 1 is shown",
      locations = cells_column_labels(columns = 7)
    )

  return(prior_prediction_summary)
}

prediction_visual <- function(model) {
  model <- model

  draws <- model |>
    spread_draws(b_ich_lateralityRight) |>
    mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
    summarize()

  pred <- predictions(model, by = "ich_laterality") |>
    tibble()

  theme_ich <- function(base_family = "Inter",
                        base_size = 14) {
    theme_minimal(base_size = base_size) +
      theme(
        text = element_text(lineheight = 2),
        plot.title = element_text(size = rel(1.3), margin = margin(12, 0, 8, 0)),
        plot.subtitle = element_text(size = rel(1.1), margin = margin(4, 0, 0, 0)),
        axis.text.y = element_text(size = rel(0.8)),
        axis.title.y = element_text(size = 12, margin = margin(0, 4, 0, 0)),
        axis.text.x = element_text(size = rel(2)),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.justification = 1,
        panel.grid = element_line(colour = "#F3F4F5"),
        plot.caption = element_text(size = rel(0.8), margin = margin(8, 0, 0, 0)),
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm")
      )
  }

  p1 <- predictions(model, by = "ich_laterality") |>
    posterior_draws() |>
    ggplot(aes(x = draw, fill = ich_laterality)) +
    geom_density(alpha = .7) +
    scale_fill_manual(values = c("#054224", "#ff8800")) +
    scale_x_continuous(labels = scales::percent) +
    labs(
      title = "Predicted Probability",
      fill = "ICH Laterality",
      y = NULL
    ) +
    theme_ich()

  return(p1)
}

f_post_predictive_check <- function(mod) {
  pp_check <- pp_check(mod, ndraws = 500)

  pp_check_bars <- pp_check(mod, ndraws = 500, type = "bars_grouped", group = "ich_laterality")

  return(pp_check / pp_check_bars)
}