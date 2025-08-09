f_imp_sensitivity <- function(neurosurgery, evd, mrs) {
  # gather OR, 95% credible intervals, and probabilities of OR's for each model
  neurosurgery_post <- neurosurgery |>
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
      or_ci = glue(
        "{round(or, digits = 2)} ({round(lower_95_ci, digits = 2)} - {round(upper_95_ci, digits = 2)})"
      )
    ) |>
    select(or_ci, or_1, or_1.2, rope)

  evd_post <- evd |>
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
      or_ci = glue(
        "{round(or, digits = 2)} ({round(lower_95_ci, digits = 2)} - {round(upper_95_ci, digits = 2)})"
      )
    ) |>
    select(or_ci, or_1, or_1.2, rope)

  mrs_post <- mrs |>
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
      or_ci = glue(
        "{round(or, digits = 2)} ({round(lower_95_ci, digits = 2)} - {round(upper_95_ci, digits = 2)})"
      )
    ) |>
    select(or_ci, or_1, or_1.2, rope)

  table <- bind_rows(
    "Neurosurgical Intervention" = neurosurgery_post,
    "External Ventricular Drain" = evd_post,
    "Modified Rankin Score" = mrs_post,
    .id = "Outcome"
  ) |>
    gt() |>
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
      2 ~ px(150),
      3:5 ~ px(125)
    ) |>
    cols_align(align = "left") |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_stub(rows = everything())
    ) |>
    tab_footnote(
      footnote = "aOR = adjusted odds ratio, CI = 95% credible interval; adjusted for age, admission GCS, ICH location, ICH volume, IVH, and study (as random intercept)",
      locations = cells_column_labels(columns = or_ci)
    ) |>
    tab_footnote(
      footnote = "ROPE = region of practical equivalence, defined as 0.95 > aOR > 1.05",
      locations = cells_column_labels(columns = rope)
    )

  return(table)
}
