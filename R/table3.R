outcome_models_list_func <- function(a, b, c, d, e, f) {
  list(
    "Modified Rankin Score" = a,
    "EuroQOL - Mobility" = b,
    "EuroQOL - Self-Care" = c,
    "EuroQOL - Usual Activities" = d,
    "EuroQOL - Pain/Discomfort" = e,
    "EuroQOL - Anxiety/Depression" = f
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
    .id = "Outcome") |>
  gt(rowname_col = "Outcome") |>
  tab_stubhead(label = "Outcome") |>
  cols_label(
    Outcome = md("**Outcome**"),
    or_ci = md("**aOR (95% CI)**"),
    or_1 = md("**Probability of any difference (aOR > 1)**"),
    or_1.2 = md("**Probability of a substantial difference (aOR > 1.2)**"),
    rope = md("**ROPE**")
  ) |>
  fmt_number(columns = 2:5, decimals = 2) |>
    cols_width(
      Outcome ~ px(375),
      2 ~ px(200),
      3:5 ~ px(150)
    ) |>
  cols_align(align = "left") |>
  tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_stub(rows = everything())
    ) |>
      tab_footnote(
        footnote = "aOR = adjusted odds ratio, CI = 95% credible interval; adjusted for age, admission GCS, ICH location, IVH, and study (as random intercept)",
        locations = cells_column_labels(columns = or_ci)
      ) |>
      tab_footnote(
        footnote = "ROPE = region of practical equivalence, defined as 0.95 > aOR > 1.05",
        locations = cells_column_labels(columns = rope)
      ) 
  return(total_tibble)
}