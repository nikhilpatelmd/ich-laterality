# R/table2.R

# Main Table Generation Function
table_2_function <- function(x, models) {
  # --- 1. Gather Raw Counts (Descriptive Stats) ---

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
    filter(days_mechanical_ventilation > 0) |> # among ventilated patients only
    group_by(ich_laterality) |>
    summarize(
      n = n(),
      median = round(median(as.numeric(days_mechanical_ventilation))),
      lower_25 = round(quantile(as.numeric(days_mechanical_ventilation), 0.25)),
      upper_75 = round(quantile(as.numeric(days_mechanical_ventilation), 0.75))
    ) |>
    mutate(
      iqr = glue::glue(
        "{median} ({lower_25} – {upper_75})"
      )
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

  # Combine descriptive stats
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

  # --- 2. Calculate Marginal Effects ---

  # Helper function for marginal effects extraction
  get_marginal_stats <- function(model) {
    fam <- stats::family(model)$family

    if (fam %in% c("bernoulli", "binomial")) {
      cmp <- "lnoravg"
    } else {
      cmp <- function(hi, lo) hi / lo
    }

    draws <- marginaleffects::avg_comparisons(
      model,
      variables = "ich_laterality",
      comparison = cmp
    ) |>
      marginaleffects::posterior_draws()

    if (fam %in% c("bernoulli", "binomial")) {
      draws <- draws |> mutate(draw = exp(draw))
    }

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
        ),
        # Apply the new formatting function here
        or_1 = format_posterior_prob(or_1),
        or_1.2 = format_posterior_prob(or_1.2),
        rope = format_posterior_prob(rope)
      ) |>
      select(or_ci, or_1, or_1.2, rope)
  }

  neurosurgery_post <- get_marginal_stats(models$"Neurosurgical Intervention")
  evd_post <- get_marginal_stats(models$"External Ventricular Drain")
  tracheostomy_post <- get_marginal_stats(models$"Tracheostomy")
  comfort_care_binary_post <- get_marginal_stats(
    models$"Withdrawal of Life-Sustaining Therapy"
  )
  early_wlst_post <- get_marginal_stats(
    models$"Early Withdrawal of Life-Sustaining Therapy"
  )
  dnr_binary_post <- get_marginal_stats(models$"DNR Order")
  days_mechanical_ventilation_post <- get_marginal_stats(
    models$"Days of Mechanical Ventilation"
  )

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

  table_2 <- total_n |>
    left_join(total_stats, by = "Outcome") |>
    gt(rowname_col = "Outcome") |>
    tab_stubhead(label = "Outcome") |>
    cols_label(
      Outcome = md("**Outcome**"),
      Left = md("**Left Hemisphere**"),
      Right = md("**Right Hemisphere**"),
      or_ci = md("**aOR / IRR (95% CrI)**"),
      or_1 = md("**Probability of difference (Est > 1)**"),
      or_1.2 = md("**Probability of substantial difference (Est > 1.2)**"),
      rope = md("**Percentage of posterior within ROPE**")
    ) |>
    cols_width(Outcome ~ px(375), 2:3 ~ px(175), 4 ~ px(150), 5:7 ~ px(125)) |>
    # 1. Align all columns left explicitly
    cols_align(align = "left", columns = everything()) |>
    # 2. Align the overall table left on the page
    tab_options(table.align = "left") |>
    tab_style(
      style = cell_text(weight = "bold", align = "left"),
      locations = cells_stub(rows = everything())
    ) |>
    tab_source_note(
      source_note = "Values represent Average Marginal Effects (Odds Ratios for binary outcomes, Incidence Rate Ratios for counts). Models are adjusted for age, admission GCS, ICH location (interaction), ICH volume, IVH, and study. Days of mechanical ventilation represent median (IQR) days (data from ATACH-2 only). ROPE indicates region of practical equivalence (0.95 to 1.05); aOR, adjusted odds ratio; IRR, incidence rate ratio; CrI, credible interval; GCS, Glasgow Coma Scale; ICH, intracerebral hemorrhage; IQR, interquartile range; and IVH, intraventricular hemorrhage."
    )

  return(table_2)
}

# --- HELPER: Subset Models for Table 2 ---
subset_models_for_table2 <- function(
  all_models,
  scenario,
  prefix = "model_main_"
) {
  target_map <- list(
    "neurosurgery_evac" = "Neurosurgical Intervention",
    "evd" = "External Ventricular Drain",
    "days_mechanical_ventilation" = "Days of Mechanical Ventilation",
    "tracheostomy" = "Tracheostomy",
    "comfort_care_binary" = "Withdrawal of Life-Sustaining Therapy",
    "early_wlst" = "Early Withdrawal of Life-Sustaining Therapy",
    "dnr_binary" = "DNR Order"
  )

  suffix <- paste0("_", scenario, "_adjusted")
  selected_models <- list()

  for (outcome_col in names(target_map)) {
    target_name <- paste0(prefix, outcome_col, suffix)

    if (
      outcome_col == "days_mechanical_ventilation" && prefix == "model_sens_"
    ) {
      target_name <- paste0("model_main_", outcome_col, suffix)
    }

    if (!target_name %in% names(all_models)) {
      stop(paste(
        "Error in Table 2 creation: Could not find target",
        target_name,
        "in the combined list."
      ))
    }

    pretty_name <- target_map[[outcome_col]]
    selected_models[[pretty_name]] <- all_models[[target_name]]
  }

  return(selected_models)
}

# --- HELPER: Subset Prior Models for Table 2 ---
subset_prior_models_for_table2 <- function(all_prior_models, scenario) {
  target_map <- list(
    "neurosurgery_evac" = "Neurosurgical Intervention",
    "evd" = "External Ventricular Drain",
    "days_mechanical_ventilation" = "Days of Mechanical Ventilation",
    "tracheostomy" = "Tracheostomy",
    "comfort_care_binary" = "Withdrawal of Life-Sustaining Therapy",
    "early_wlst" = "Early Withdrawal of Life-Sustaining Therapy",
    "dnr_binary" = "DNR Order"
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

# --- Main Prior Table Generation Function ---
table_2_priors_function <- function(models) {
  get_prior_stats <- function(model) {
    fam <- stats::family(model)$family
    is_binary <- fam %in% c("bernoulli", "binomial")

    preds <- marginaleffects::avg_predictions(model, by = "ich_laterality") |>
      marginaleffects::posterior_draws()

    if (!is_binary) {
      pred_summary <- preds |>
        group_by(ich_laterality) |>
        summarize(
          val = glue::glue(
            "{round(median(draw), 1)} ({round(quantile(draw, 0.25), 1)} - {round(quantile(draw, 0.75), 1)})"
          ),
          .groups = "drop"
        ) |>
        pivot_wider(names_from = ich_laterality, values_from = val)
    } else {
      pred_summary <- preds |>
        group_by(ich_laterality) |>
        summarize(
          val = glue::glue(
            "{sprintf('%.1f', median(draw)*100)}% ({sprintf('%.1f', quantile(draw, 0.025)*100)} - {sprintf('%.1f', quantile(draw, 0.975)*100)}%)"
          ),
          .groups = "drop"
        ) |>
        pivot_wider(names_from = ich_laterality, values_from = val)
    }

    cmp <- if (is_binary) "lnoravg" else function(hi, lo) hi / lo

    comps <- marginaleffects::avg_comparisons(
      model,
      variables = "ich_laterality",
      comparison = cmp
    ) |>
      marginaleffects::posterior_draws()

    if (is_binary) {
      comps <- comps |> mutate(draw = exp(draw))
    }

    comp_summary <- comps |>
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
        ),
        # Apply the new formatting function here as well
        or_1 = format_posterior_prob(or_1),
        or_1.2 = format_posterior_prob(or_1.2),
        rope = format_posterior_prob(rope)
      ) |>
      select(or_ci, or_1, or_1.2, rope)

    bind_cols(pred_summary, comp_summary)
  }

  total_stats <- bind_rows(
    "Neurosurgical Intervention" = get_prior_stats(
      models$"Neurosurgical Intervention"
    ),
    "External Ventricular Drain" = get_prior_stats(
      models$"External Ventricular Drain"
    ),
    "Days of Mechanical Ventilation" = get_prior_stats(
      models$"Days of Mechanical Ventilation"
    ),
    "Tracheostomy" = get_prior_stats(models$"Tracheostomy"),
    "Withdrawal of Life-Sustaining Therapy" = get_prior_stats(
      models$"Withdrawal of Life-Sustaining Therapy"
    ),
    "Early Withdrawal of Life-Sustaining Therapy" = get_prior_stats(
      models$"Early Withdrawal of Life-Sustaining Therapy"
    ),
    "DNR Order" = get_prior_stats(models$"DNR Order"),
    .id = "Outcome"
  )

  # --- 3. Final GT Table Construction ---
  table_2_priors <- total_stats |>
    gt(rowname_col = "Outcome") |>
    tab_stubhead(label = "Outcome") |>
    cols_label(
      Outcome = md("**Outcome**"),
      Left = md("**Left Hemisphere (Prior expected)**"),
      Right = md("**Right Hemisphere (Prior expected)**"),
      or_ci = md("**Prior aOR / IRR (95% CrI)**"),
      or_1 = md("**Probability of difference (aOR > 1)**"),
      or_1.2 = md("**Probability of substantial difference (aOR > 1.2)**"),
      rope = md("**Percentage of posterior within ROPE**")
    ) |>
    cols_width(Outcome ~ px(375), 2:3 ~ px(200), 4 ~ px(150), 5:7 ~ px(125)) |>
    # 1. Align all columns left explicitly
    cols_align(align = "left", columns = everything()) |>
    # 2. Align the overall table left on the page
    tab_options(table.align = "left") |>
    tab_style(
      style = cell_text(weight = "bold", align = "left"),
      locations = cells_stub(rows = everything())
    ) |>
    tab_source_note(
      source_note = "Values represent expected values simulated purely from the prior distributions before encountering the data. ROPE indicates region of practical equivalence (0.95 to 1.05); aOR, adjusted odds ratio; IRR, incidence rate ratio; and CrI, credible interval."
    )
}