table_atach_function <- function(data, model_base, model_site) {
  # --- 1. Internal Helper: Marginal Stats (Same as Table 2) ---
  get_marginal_stats <- function(model) {
    # Detect Family (Bernoulli only for this table)
    # We assume log-odds for binary

    draws <- marginaleffects::avg_comparisons(
      model,
      variables = "ich_laterality",
      comparison = "lnoravg" # Log Odds Ratio
    ) |>
      marginaleffects::posterior_draws() |>
      mutate(draw = exp(draw)) # Exponentiate to get OR

    draws |>
      rename(effect_ratio = draw) |>
      summarize(
        or = median(effect_ratio),
        lower_95_ci = quantile(effect_ratio, 0.025),
        upper_95_ci = quantile(effect_ratio, 0.975),
        or_1 = sum(effect_ratio > 1) / n(),
        or_1.2 = sum(effect_ratio > 1.2) / n(),
        rope = sum(effect_ratio < 1.05 & effect_ratio > 0.95) / n()
      ) |>
      mutate(
        or_ci = glue::glue(
          "{round(or, 2)} ({round(lower_95_ci, 2)} - {round(upper_95_ci, 2)})"
        )
      ) |>
      select(or_ci, or_1, or_1.2, rope)
  }

  # --- 2. Calculate Counts (Common to both rows) ---
  # Using the specific ATACH subset data
  neuro_counts <- data |>
    janitor::tabyl(ich_laterality, neurosurgery_evac) |>
    janitor::adorn_percentages("row") |>
    janitor::adorn_pct_formatting(digits = 1) |>
    janitor::adorn_ns(position = "front") |>
    select(!No) |>
    pivot_wider(names_from = ich_laterality, values_from = Yes)

  # --- 3. Get Stats for Both Models ---
  stats_base <- get_marginal_stats(model_base)
  stats_site <- get_marginal_stats(model_site)

  # --- 4. Combine into Rows ---
  row_base <- bind_cols(
    tibble(Model = "Base Model (No Site Adjustment)"),
    neuro_counts,
    stats_base
  )

  row_site <- bind_cols(
    tibble(Model = "Site Sensitivity (Random Intercept)"),
    neuro_counts,
    stats_site
  )

  final_df <- bind_rows(row_base, row_site)

  # --- 5. Generate GT Table ---
  final_df |>
    gt(rowname_col = "Model") |>
    tab_stubhead(label = "Model Specification") |>
    cols_label(
      Model = md("**Model Specification**"),
      Left = md("**Left Hemisphere**"),
      Right = md("**Right Hemisphere**"),
      or_ci = md("**aOR (95% CI)**"),
      or_1 = md("**Prob > 1**"),
      or_1.2 = md("**Prob > 1.2**"),
      rope = md("**ROPE**")
    ) |>
    fmt_number(columns = 5:7, decimals = 2) |>
    cols_align(align = "left") |>
    tab_header(
      title = "Sensitivity Analysis: Impact of Site Clustering",
      subtitle = "Outcome: Neurosurgical Intervention (ATACH-2 Cohort)"
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_stub(rows = everything())
    )
}
