library(tidyverse)
library(tidybayes)
library(brms)
library(gt)
library(glue)

# --- HELPER: Universal Subset Models ---
subset_models_for_table4 <- function(
  all_models,
  scenario,
  prefix = "model_main_"
) {
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
    target_name <- paste0(prefix, outcome_col, suffix)

    if (!target_name %in% names(all_models)) {
      stop(paste("Could not find target", target_name, "in combined list."))
    }

    selected_models[[target_map[[outcome_col]]]] <- all_models[[target_name]]
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
      lower = unname(quantile(estim, 0.025)),
      upper = unname(quantile(estim, 0.975)),
      prob_diff = mean(is_diff),
      prob_sub = mean(is_substantial),
      prob_rope = mean(in_rope)
    ) |>
    mutate(
      outcome = label,
      est_label = as.character(glue::glue(
        "{sprintf('%.2f', est_median)} ({sprintf('%.2f', lower)} - {sprintf('%.2f', upper)})"
      ))
    )
}

# --- 2. Helper function for VAS ZOIB Model (Mean Difference) ---
process_vas <- function(model, label, data) {
  # 1. Clean dataset natively. Drop NAs ONLY for variables used in this specific model.
  used_vars <- setdiff(all.vars(stats::formula(model)$formula), "euro_vas_90")
  clean_data <- as.data.frame(data) |>
    tidyr::drop_na(dplyr::any_of(used_vars))

  # 2. Create the hypothetical data grids
  data_left <- clean_data
  data_left$ich_laterality <- factor("Left", levels = c("Left", "Right"))

  data_right <- clean_data
  data_right$ich_laterality <- factor("Right", levels = c("Left", "Right"))

  # 3. Native G-Computation: Get expected posterior predictions directly from brms
  # This flawlessly handles brmsfit_multiple (imputed) objects!
  pred_left <- brms::posterior_epred(model, newdata = data_left)
  pred_right <- brms::posterior_epred(model, newdata = data_right)

  # 4. Average the predictions over all patients for each MCMC draw (Average Marginal Effect)
  estim <- (rowMeans(pred_right) - rowMeans(pred_left)) * 100

  # 5. Summarize the posterior distribution of the Mean Difference
  est_median <- median(estim)
  lower <- unname(quantile(estim, 0.025, na.rm = TRUE))
  upper <- unname(quantile(estim, 0.975, na.rm = TRUE))
  prob_diff <- mean(estim < 0, na.rm = TRUE)
  prob_sub <- mean(estim < -5, na.rm = TRUE)
  prob_rope <- mean(estim > -2 & estim < 2, na.rm = TRUE)

  tibble(
    outcome = label,
    est_label = as.character(glue::glue(
      "{sprintf('%.1f', est_median)} ({sprintf('%.1f', lower)} - {sprintf('%.1f', upper)})"
    )),
    prob_diff = prob_diff,
    prob_sub = prob_sub,
    prob_rope = prob_rope
  )
}

# --- 3. Unified Table Function ---
table_4_function <- function(x, models, is_prior = FALSE) {
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

  df_vas <- process_vas(models$"Euro VAS", "Euro VAS", data = x)

  # Dynamic labels based on whether it's a prior predictive check or posterior
  est_col_label <- ifelse(
    is_prior,
    "Prior Effect Estimate (95% CI)",
    "aOR (95% CI)"
  )
  prob_diff_label <- ifelse(
    is_prior,
    "Prior Prob. of any diff (aOR > 1)",
    "Probability of any difference (aOR > 1)"
  )
  prob_sub_label <- ifelse(
    is_prior,
    "Prior Prob. of substantial diff (aOR > 1.2)",
    "Probability of a substantial difference (aOR > 1.2)"
  )
  prob_rope_label <- ifelse(is_prior, "Prior ROPE", "ROPE")

  bind_rows(df_mrs, df_mob, df_self, df_act, df_pain, df_anx, df_vas) |>
    select(outcome, est_label, prob_diff, prob_sub, prob_rope) |>
    gt::gt() |>
    gt::cols_label(
      outcome = "Outcome",
      est_label = est_col_label,
      prob_diff = prob_diff_label,
      prob_sub = prob_sub_label,
      prob_rope = prob_rope_label
    ) |>
    gt::fmt_number(
      columns = c("prob_diff", "prob_sub", "prob_rope"),
      decimals = 2
    ) |>
    gt::cols_align(align = "left", columns = "outcome") |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_footnote(
      footnote = ifelse(
        is_prior,
        "Values represent expected effects simulated purely from the prior distributions before encountering the data. Estimates are Prior Odds Ratios (aOR).",
        "aOR = adjusted odds ratio, CI = 95% credible interval; adjusted for age, admission GCS, ICH location, ICH volume, IVH, and study (as random intercept); Reference Category: Left Hemisphere Laterality"
      ),
      locations = gt::cells_column_labels(columns = "est_label")
    ) |>
    gt::tab_footnote(
      footnote = "ROPE = region of practical equivalence, defined as 0.95 to 1.05",
      locations = gt::cells_column_labels(columns = "prob_rope")
    ) |>
    gt::tab_footnote(
      footnote = "For Euro VAS: Estimate is Mean Difference in points (0-100).",
      locations = gt::cells_body(
        columns = "est_label",
        rows = outcome == "Euro VAS"
      )
    ) |>
    gt::tab_footnote(
      footnote = "Probability of Mean Difference < 0",
      locations = gt::cells_body(
        columns = "prob_diff",
        rows = outcome == "Euro VAS"
      )
    ) |>
    gt::tab_footnote(
      footnote = "Probability of Mean Difference < -5 points",
      locations = gt::cells_body(
        columns = "prob_sub",
        rows = outcome == "Euro VAS"
      )
    ) |>
    gt::tab_footnote(
      footnote = "ROPE for Mean Difference defined as +/- 2 points",
      locations = gt::cells_body(
        columns = "prob_rope",
        rows = outcome == "Euro VAS"
      )
    )
}
