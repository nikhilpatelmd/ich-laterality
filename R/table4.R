library(tidyverse)
library(tidybayes)
library(marginaleffects)
library(gt)
library(glue)

# --- HELPER: Subset Models for Table 4 ---
subset_models_for_table4 <- function(all_models, scenario) {
  # Map: Grid Outcome -> Human Name
  target_map <- list(
    "mrs_90" = "Modified Rankin Score",
    "euro_mobility_90" = "EuroQOL - Mobility",
    "euro_selfcare_90" = "EuroQOL - Self-Care",
    "euro_usual_90" = "EuroQOL - Usual Activities",
    "euro_pain_90" = "EuroQOL - Pain/Discomfort",
    "euro_anxiety_90" = "EuroQOL - Anxiety/Depression",
    "euro_vas_90" = "Euro VAS"
  )

  suffix <- paste0("_", scenario, "_adjusted")
  selected_models <- list()

  for (outcome_col in names(target_map)) {
    target_name <- paste0("model_main_", outcome_col, suffix)

    if (!target_name %in% names(all_models)) {
      stop(paste("Could not find target", target_name, "in combined list."))
    }

    pretty_name <- target_map[[outcome_col]]
    selected_models[[pretty_name]] <- all_models[[target_name]]
  }

  return(selected_models)
}

# --- 1. Helper function for Ordinal Models (Odds Ratios) ---
process_ordinal <- function(model, label) {
  model |>
    tidybayes::spread_draws(b_ich_lateralityRight) |>
    mutate(
      estim = exp(b_ich_lateralityRight),
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
      est_label = glue::glue(
        "{sprintf('%.2f', est_median)} ({sprintf('%.2f', lower)} - {sprintf('%.2f', upper)})"
      )
    )
}

# --- 2. Helper function for VAS ZOIB Model (Mean Difference) ---
process_vas <- function(model, label) {
  comp <- marginaleffects::avg_comparisons(
    model,
    variables = "ich_laterality",
    type = "response"
  )

  marginaleffects::posterior_draws(comp) |>
    mutate(
      estim = draw * 100,
      is_diff = estim < 0,
      is_substantial = estim < -5,
      in_rope = estim > -2 & estim < 2
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
      est_label = glue::glue(
        "{sprintf('%.1f', est_median)} ({sprintf('%.1f', lower)} - {sprintf('%.1f', upper)})"
      )
    )
}

# --- 3. Main Table Function ---
table_4_function <- function(models) {
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

  df_vas <- process_vas(models$"Euro VAS", "Euro VAS")

  bind_rows(df_mrs, df_mob, df_self, df_act, df_pain, df_anx, df_vas) |>
    select(outcome, est_label, prob_diff, prob_sub, prob_rope, type) |>
    gt::gt() |>
    gt::cols_label(
      outcome = gt::md("**Outcome**"),
      est_label = gt::md("**Effect Estimate (95% CI)**"),
      prob_diff = gt::md("**Prob. Any Diff**"),
      prob_sub = gt::md("**Prob. Substantial**"),
      prob_rope = gt::md("**ROPE**")
    ) |>
    gt::fmt_number(columns = c(prob_diff, prob_sub, prob_rope), decimals = 2) |>
    gt::cols_align(align = "left", columns = outcome) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_footnote(
      footnote = "For Ordinal Outcomes: Adjusted Odds Ratio (aOR). For Euro VAS: Mean Difference in points (0-100).",
      locations = gt::cells_column_labels(columns = est_label)
    ) |>
    gt::tab_footnote(
      footnote = "Any Diff: Prob(aOR > 1) or Prob(VAS Diff < 0)",
      locations = gt::cells_column_labels(columns = prob_diff)
    ) |>
    gt::tab_footnote(
      footnote = "Substantial: Prob(aOR > 1.2) or Prob(VAS Diff < -5 points)",
      locations = gt::cells_column_labels(columns = prob_sub)
    ) |>
    gt::tab_footnote(
      footnote = "ROPE: Region of Practical Equivalence. aOR (0.95-1.05) or VAS (+/- 2 points).",
      locations = gt::cells_column_labels(columns = prob_rope)
    )
}

# --- HELPER: Subset Prior Models for Table 4 ---
subset_prior_models_for_table4 <- function(all_prior_models, scenario) {
  target_map <- list(
    "mrs_90" = "Modified Rankin Score",
    "euro_mobility_90" = "EuroQOL - Mobility",
    "euro_selfcare_90" = "EuroQOL - Self-Care",
    "euro_usual_90" = "EuroQOL - Usual Activities",
    "euro_pain_90" = "EuroQOL - Pain/Discomfort",
    "euro_anxiety_90" = "EuroQOL - Anxiety/Depression",
    "euro_vas_90" = "Euro VAS"
  )

  suffix <- paste0("_", scenario, "_adjusted")
  selected_models <- list()

  for (outcome_col in names(target_map)) {
    target_name <- paste0("model_prior_", outcome_col, suffix)

    if (!target_name %in% names(all_prior_models)) {
      stop(paste(
        "Could not find prior target",
        target_name,
        "in combined list."
      ))
    }

    pretty_name <- target_map[[outcome_col]]
    selected_models[[pretty_name]] <- all_prior_models[[target_name]]
  }

  return(selected_models)
}

# --- Main Prior Table 4 Function ---
table_4_priors_function <- function(models) {
  # We can perfectly recycle your existing process_ordinal and process_vas functions!
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

  df_vas <- process_vas(models$"Euro VAS", "Euro VAS")

  bind_rows(df_mrs, df_mob, df_self, df_act, df_pain, df_anx, df_vas) |>
    select(outcome, est_label, prob_diff, prob_sub, prob_rope, type) |>
    gt::gt() |>
    gt::cols_label(
      outcome = gt::md("**Outcome**"),
      est_label = gt::md("**Prior Effect Estimate (95% CI)**"),
      prob_diff = gt::md("**Prior Prob. Any Diff**"),
      prob_sub = gt::md("**Prior Prob. Substantial**"),
      prob_rope = gt::md("**Prior ROPE**")
    ) |>
    gt::fmt_number(columns = c(prob_diff, prob_sub, prob_rope), decimals = 2) |>
    gt::cols_align(align = "left", columns = outcome) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_footnote(
      footnote = "Values represent expected effects simulated purely from the prior distributions before encountering the data.",
      locations = gt::cells_column_labels(columns = est_label)
    ) |>
    gt::tab_footnote(
      footnote = "For Ordinal Outcomes: Prior Odds Ratio (OR). For Euro VAS: Prior Mean Difference in points (0-100).",
      locations = gt::cells_column_labels(columns = est_label)
    ) |>
    gt::tab_footnote(
      footnote = "Any Diff: Prob(OR > 1) or Prob(VAS Diff < 0)",
      locations = gt::cells_column_labels(columns = prob_diff)
    ) |>
    gt::tab_footnote(
      footnote = "Substantial: Prob(OR > 1.2) or Prob(VAS Diff < -5 points)",
      locations = gt::cells_column_labels(columns = prob_sub)
    ) |>
    gt::tab_footnote(
      footnote = "ROPE: Region of Practical Equivalence. OR (0.95-1.05) or VAS (+/- 2 points).",
      locations = gt::cells_column_labels(columns = prob_rope)
    )
}
