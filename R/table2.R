models_list_func <- function(a, b, c, d, e, f, g) {
  list(
    "Neurosurgical Intervention" = a,
    "External Ventricular Drain" = b,
    "Days of Mechanical Ventilation" = c,
    "Tracheostomy" = d,
    "Withdrawal of Life-Sustaining Therapy" = e,
    "Early Withdrawal of Life-Sustaining Therapy" = f,
    "DNR Order" = g
  )
}

table_2_function <- function(x, models) {
  models <- models

  # gather raw counts for each outcome
  neurosurgery_n <- x |>
    tabyl(ich_laterality, neurosurgery_evac) |>
    adorn_percentages("row") |>
    adorn_pct_formatting(digits = 1) |>
    adorn_ns(position = "front") |>
    select(!No) |>
    pivot_wider(names_from = ich_laterality, values_from = Yes)

  evd_n <- x |>
    tabyl(ich_laterality, evd) |>
    adorn_percentages("row") |>
    adorn_pct_formatting(digits = 1) |>
    adorn_ns(position = "front") |>
    select(!No) |>
    pivot_wider(names_from = ich_laterality, values_from = Yes)

  days_mechanical_ventilation_n <- x |>
    drop_na(days_mechanical_ventilation) |>
    group_by(ich_laterality) |>
    summarize(
      median = median(as.numeric(days_mechanical_ventilation)),
      lower_25 = quantile(as.numeric(days_mechanical_ventilation), 0.25),
      upper_75 = round(quantile(as.numeric(days_mechanical_ventilation), 0.75))
    ) |>
    mutate(
      iqr = glue("{median} ({lower_25} - {upper_75})")
    ) |>
    select(ich_laterality, iqr) |>
    pivot_wider(names_from = ich_laterality, values_from = iqr)

  tracheostomy_n <- x |>
    tabyl(ich_laterality, tracheostomy) |>
    adorn_percentages("row") |>
    adorn_pct_formatting(digits = 1) |>
    adorn_ns(position = "front") |>
    select(!No) |>
    pivot_wider(names_from = ich_laterality, values_from = Yes)

  comfort_care_binary_n <- x |>
    tabyl(ich_laterality, comfort_care_binary) |>
    adorn_percentages("row") |>
    adorn_pct_formatting(digits = 1) |>
    adorn_ns(position = "front") |>
    select(!No) |>
    pivot_wider(names_from = ich_laterality, values_from = Yes)

  early_wlst_n <- x |>
    tabyl(ich_laterality, early_wlst) |>
    adorn_percentages("row") |>
    adorn_pct_formatting(digits = 1) |>
    adorn_ns(position = "front") |>
    select(!No) |>
    pivot_wider(names_from = ich_laterality, values_from = Yes)

  dnr_binary_n <- x |>
    tabyl(ich_laterality, dnr_binary) |>
    adorn_percentages("row") |>
    adorn_pct_formatting(digits = 1) |>
    adorn_ns(position = "front") |>
    select(!No) |>
    pivot_wider(names_from = ich_laterality, values_from = Yes)

  # convert counts to proper formatting before adding stats
  total_n <- bind_rows(
    "Neurosurgical Intervention" = neurosurgery_n,
    "External Ventricular Drain" = evd_n,
    "Days of Mechanical Ventilation" = days_mechanical_ventilation_n,
    "Tracheostomy" = tracheostomy_n,
    "Withdrawal of Life-Sustaining Therapy" = comfort_care_binary_n,
    "Early Withdrawal of Life-Sustaining Therapy" = early_wlst_n,
    "DNR Order" = dnr_binary_n,
    .id = "Outcome"
  )

  # gather OR, 95% credible intervals, and probabilities of OR's for each model
  neurosurgery_post <- models$"Neurosurgical Intervention" |>
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

  evd_post <- models$"External Ventricular Drain" |>
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

  days_mechanical_ventilation_post <- models$"Days of Mechanical Ventilation" |>
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

  tracheostomy_post <- models$"Tracheostomy" |>
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

  comfort_care_binary_post <- models$"Withdrawal of Life-Sustaining Therapy" |>
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

  early_wlst_post <- models$"Early Withdrawal of Life-Sustaining Therapy" |>
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

  dnr_binary_post <- models$"DNR Order" |>
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

  # Combine stats into a single table before combining with raw counts
  total_stats <- bind_rows(
    "Neurosurgical Intervention" = neurosurgery_post,
    "External Ventricular Drain" = evd_post,
    "Days of Mechanical Ventilation" = days_mechanical_ventilation_post,
    "Tracheostomy" = tracheostomy_post,
    "Withdrawal of Life-Sustaining Therapy" = comfort_care_binary_post,
    "Early Withdrawal of Life-Sustaining Therapy" = early_wlst_post,
    "DNR Order" = dnr_binary_post,
    .id = "Outcome"
  )

  # Combine raw counts and stats into final table

  table_2 <- total_n |>
    left_join(total_stats, by = "Outcome") |>
    gt(rowname_col = "Outcome") |>
    tab_stubhead(label = "Outcome") |>
    cols_label(
      Outcome = md("**Outcome**"),
      Left = md("**Left Hemisphere**"),
      Right = md("**Right Hemisphere**"),
      or_ci = md("**aOR (95% CI)**"),
      or_1 = md("**Probability of any difference (aOR > 1)**"),
      or_1.2 = md("**Probability of a substantial difference (aOR > 1.2)**"),
      rope = md("**ROPE**")
    ) |>
    fmt_number(columns = 5:7, decimals = 2) |>
    cols_width(
      Outcome ~ px(375),
      2:3 ~ px(175),
      4 ~ px(150),
      5:7 ~ px(125)
    ) |>
    cols_align(align = "left") |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_stub(rows = everything())
    ) |>
    tab_footnote(
      footnote = "median (IQR)",
      locations = cells_body(columns = 2:3, rows = 3)
    ) |>
    tab_footnote(
      footnote = "aOR = adjusted odds ratio, CI = 95% credible interval; adjusted for age, admission GCS, ICH location, IVH, and study (as random intercept)",
      locations = cells_column_labels(columns = or_ci)
    ) |>
    tab_footnote(
      footnote = "ROPE = region of practical equivalence, defined as 0.95 > aOR > 1.05",
      locations = cells_column_labels(columns = rope)
    )
  return(table_2)
}
