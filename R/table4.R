library(tidyverse)
library(brms)
library(tidybayes)
library(marginaleffects)
library(gt)
library(glue)

# 1. Helper function for Ordinal Models (Odds Ratios)
process_ordinal <- function(model, label) {
  model |>
    spread_draws(b_ich_lateralityRight) |>
    mutate(
      # Exponentiate to get Odds Ratio
      estim = exp(b_ich_lateralityRight),
      # Define what "Substantial" means for OR (e.g. OR > 1.2)
      # Assuming "Right" is the risk factor we are testing
      is_diff = estim > 1,
      is_substantial = estim > 1.2,
      in_rope = estim < 1.05 & estim > 0.95
    ) |>
    summarize(
      est_median = median(estim),
      lower = quantile(estim, 0.025),
      upper = quantile(estim, 0.975),
      prob_diff = mean(is_diff),
      prob_sub = mean(is_substantial),
      prob_rope = mean(in_rope)
    ) |>
    mutate(
      outcome = label,
      type = "OR",
      est_label = glue(
        "{sprintf('%.2f', est_median)} ({sprintf('%.2f', lower)} - {sprintf('%.2f', upper)})"
      )
    )
}

# 2. Helper function for VAS ZOIB Model (Mean Difference)
process_vas <- function(model, label) {
  # Use marginaleffects to get the Expected Value difference (Right - Left)
  # type = "response" integrates mu, phi, zoi, and coi into one mean score (0-1)
  comp <- avg_comparisons(
    model,
    variables = "ich_laterality",
    type = "response"
  )

  # Extract posterior draws of this difference
  posterior_draws(comp) |>
    mutate(
      # Convert 0-1 scale to 0-100 points
      estim = draw * 100,

      # VAS LOGIC: Negative diff means Right is WORSE (Lower score).
      # Adjust these inequalities if you want to test "Right is Better"
      is_diff = estim < 0, # Prob that Right is worse than Left
      is_substantial = estim < -5, # Prob that Right is >5 points worse (MCID)
      in_rope = estim > -2 & estim < 2 # Region of Practical Equivalence (+/- 2 pts)
    ) |>
    summarize(
      est_median = median(estim),
      lower = quantile(estim, 0.025),
      upper = quantile(estim, 0.975),
      prob_diff = mean(is_diff),
      prob_sub = mean(is_substantial),
      prob_rope = mean(in_rope)
    ) |>
    mutate(
      outcome = label,
      type = "Diff",
      est_label = glue(
        "{sprintf('%.1f', est_median)} ({sprintf('%.1f', lower)} - {sprintf('%.1f', upper)})"
      )
    )
}

# 3. Main Table Function
table_3_updated <- function(models) {
  # A. Process Ordinal Models
  # (List your specific model objects here mapped to names)
  df_mrs <- process_ordinal(
    models$"Modified Rankin Score",
    "Modified Rankin Score"
  )
  df_mob <- process_ordinal(models$"EuroQOL - Mobility", "EuroQOL - Mobility")
  df_self <- process_ordinal(
    models$"EuroQOL - Self-Care",
    "EuroQOL - Self-Care"
  )
  df_act <- process_ordinal(
    models$"EuroQOL - Usual Activities",
    "EuroQOL - Usual Activities"
  )
  df_pain <- process_ordinal(
    models$"EuroQOL - Pain/Discomfort",
    "EuroQOL - Pain/Discomfort"
  )
  df_anx <- process_ordinal(
    models$"EuroQOL - Anxiety/Depression",
    "EuroQOL - Anxiety/Depression"
  )

  # B. Process VAS Model (The ZOIB model)
  df_vas <- process_vas(models$"Euro VAS", "Euro VAS")

  # C. Combine
  bind_rows(df_mrs, df_mob, df_self, df_act, df_pain, df_anx, df_vas) |>
    select(outcome, est_label, prob_diff, prob_sub, prob_rope, type) |>
    gt() |>
    cols_label(
      outcome = md("**Outcome**"),
      est_label = md("**Effect Estimate (95% CI)**"),
      prob_diff = md("**Prob. Any Diff**"),
      prob_sub = md("**Prob. Substantial**"),
      prob_rope = md("**ROPE**")
    ) |>
    fmt_number(columns = c(prob_diff, prob_sub, prob_rope), decimals = 2) |>
    cols_align(align = "left", columns = outcome) |>

    # D. Formatting & Footnotes
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) |>
    tab_footnote(
      footnote = "For Ordinal Outcomes: Adjusted Odds Ratio (aOR). For Euro VAS: Mean Difference in points (0-100).",
      locations = cells_column_labels(columns = est_label)
    ) |>
    tab_footnote(
      footnote = "Any Diff: Prob(aOR > 1) or Prob(VAS Diff < 0)",
      locations = cells_column_labels(columns = prob_diff)
    ) |>
    tab_footnote(
      footnote = "Substantial: Prob(aOR > 1.2) or Prob(VAS Diff < -5 points)",
      locations = cells_column_labels(columns = prob_sub)
    ) |>
    tab_footnote(
      footnote = "ROPE: Region of Practical Equivalence. aOR (0.95-1.05) or VAS (+/- 2 points).",
      locations = cells_column_labels(columns = prob_rope)
    )
}
