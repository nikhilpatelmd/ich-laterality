# --- HELPER: Universal Subset Models ---
subset_models_for_table3 <- function(
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

# --- 1. Helper function for Ordinal Models (Population-Averaged Odds Ratios) ---
process_ordinal <- function(model, label) {
  marginaleffects::avg_comparisons(
    model,
    variables = "ich_laterality",
    comparison = "lnoravg"
  ) |>
    marginaleffects::posterior_draws() |>
    group_by(drawid) |>
    summarize(log_or = mean(draw, na.rm = TRUE), .groups = "drop") |>
    # Drop draws where marginaleffects couldn't compute the contrast
    # (can occur in brmsfit_multiple objects for some imputed datasets)
    filter(!is.na(log_or)) |>
    mutate(estim = exp(log_or)) |>
    summarize(
      est_median = median(estim),
      lower = unname(quantile(estim, 0.025)),
      upper = unname(quantile(estim, 0.975)),
      prob_diff = mean(estim > 1),
      prob_sub = mean(estim > 1.2),
      prob_rope = mean(estim < 1.05 & estim > 0.95)
    ) |>
    mutate(
      outcome = label,
      est_label = as.character(glue::glue(
        "{sprintf('%.2f', est_median)} ({sprintf('%.2f', lower)} - {sprintf('%.2f', upper)})"
      )),
      # Apply formatting function to probabilities
      prob_diff = format_posterior_prob(prob_diff),
      prob_sub = format_posterior_prob(prob_sub),
      prob_rope = format_posterior_prob(prob_rope)
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
    # Apply formatting function to probabilities
    prob_diff = format_posterior_prob(prob_diff),
    prob_sub = format_posterior_prob(prob_sub),
    prob_rope = format_posterior_prob(prob_rope)
  )
}

# --- 3. Unified Table Function ---
table_3_function <- function(x, models, is_prior = FALSE) {
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
    "Prior Effect Estimate (95% CrI)",
    "aOR / Mean Diff (95% CrI)"
  )
  prob_diff_label <- ifelse(
    is_prior,
    "Prior Prob. of any diff",
    "Probability of any difference"
  )
  prob_sub_label <- ifelse(
    is_prior,
    "Prior Prob. of substantial diff",
    "Probability of a substantial difference"
  )
  prob_rope_label <- ifelse(
    is_prior,
    "Percentage of prior within ROPE",
    "Percentage of posterior within ROPE"
  )

  # Consolidated Source Note perfectly matching the Style of Table 1 & 2
  source_note_text <- ifelse(
    is_prior,
    "Values represent expected effects simulated purely from the prior distributions before encountering the data. Estimates are Prior Odds Ratios (aOR) for ordinal outcomes and Mean Differences for Euro VAS. ROPE indicates region of practical equivalence (0.95 to 1.05 for aOR; ± 2 points for Euro VAS).",
    "Values represent Average Marginal Effects (Odds Ratios for ordinal outcomes; Mean Difference in points [0–100] for Euro VAS). Models are adjusted for age, admission GCS, ICH location, ICH volume, IVH, and study (as random intercept); Reference Category: Left Hemisphere. ROPE indicates region of practical equivalence (0.95 to 1.05 for ordinal; ± 2 points for Euro VAS). For Euro VAS, probability of difference is defined as Mean Difference < 0, and substantial difference as < -5 points. aOR indicates adjusted odds ratio; CrI, credible interval; GCS, Glasgow Coma Scale; ICH, intracerebral hemorrhage; and IVH, intraventricular hemorrhage."
  )

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
    # 1. Align all columns left
    gt::cols_align(align = "left", columns = gt::everything()) |>
    # 2. Align the overall table left on the page
    gt::tab_options(table.align = "left") |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(columns = "outcome")
    ) |>
    gt::tab_source_note(
      source_note = source_note_text
    )
}
