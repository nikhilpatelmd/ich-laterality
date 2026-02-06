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
    group_by(ich_laterality) |>
    summarize(
      median = median(as.numeric(days_mechanical_ventilation)),
      lower_25 = quantile(as.numeric(days_mechanical_ventilation), 0.25),
      upper_75 = round(quantile(as.numeric(days_mechanical_ventilation), 0.75))
    ) |>
    mutate(
      iqr = glue::glue("{median} ({lower_25} - {upper_75})")
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
  get_marginal_stats <- function(model, comparison_type = "ratio") {
    marginaleffects::avg_comparisons(
      model,
      variables = "ich_laterality",
      comparison = function(hi, lo) hi / lo # Ratio: Right / Left
    ) |>
      marginaleffects::posterior_draws() |>
      rename(effect_ratio = draw) |>
      summarize(
        or = median(effect_ratio),
        lower_95_ci = quantile(effect_ratio, 0.025),
        upper_95_ci = quantile(effect_ratio, 0.975),
        or_1 = sum(effect_ratio > 1) / n(), # Prob > 1
        or_1.2 = sum(effect_ratio > 1.2) / n(), # Prob > 1.2
        rope = sum(effect_ratio < 1.05 & effect_ratio > 0.95) / n() # ROPE
      ) |>
      mutate(
        or_ci = glue::glue(
          "{round(or, 2)} ({round(lower_95_ci, 2)} - {round(upper_95_ci, 2)})"
        )
      ) |>
      select(or_ci, or_1, or_1.2, rope)
  }

  # Apply to models (Using the Pretty Names from the list)
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

  # Count model (IRR)
  days_mechanical_ventilation_post <- get_marginal_stats(
    models$"Days of Mechanical Ventilation"
  )

  # Combine stats
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

  # --- 3. Final GT Table Construction ---
  table_2 <- total_n |>
    left_join(total_stats, by = "Outcome") |>
    gt(rowname_col = "Outcome") |>
    tab_stubhead(label = "Outcome") |>
    cols_label(
      Outcome = md("**Outcome**"),
      Left = md("**Left Hemisphere**"),
      Right = md("**Right Hemisphere**"),
      or_ci = md("**aOR / IRR (95% CI)**"),
      or_1 = md("**Probability of difference (Est > 1)**"),
      or_1.2 = md("**Probability of substantial difference (Est > 1.2)**"),
      rope = md("**ROPE**")
    ) |>
    fmt_number(columns = 5:7, decimals = 2) |>
    cols_width(Outcome ~ px(375), 2:3 ~ px(175), 4 ~ px(150), 5:7 ~ px(125)) |>
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
      footnote = "Values represent Average Marginal Effects (Odds Ratios for binary, Rate Ratios for counts). Adjusted for age, admission GCS, ICH location (interaction), ICH volume, IVH, and study.",
      locations = cells_column_labels(columns = or_ci)
    ) |>
    tab_footnote(
      footnote = "ROPE = region of practical equivalence (0.95 to 1.05)",
      locations = cells_column_labels(columns = rope)
    )

  return(table_2)
}

# --- HELPER: Subset Models for Table 2 ---
# This function plucks the correct models from the massive tar_combine list
subset_models_for_table2 <- function(all_models, scenario) {
  # 1. Map: Machine Name (Grid Outcome) -> Human Name (Table 2 Row)
  target_map <- list(
    "neurosurgery_evac" = "Neurosurgical Intervention",
    "evd" = "External Ventricular Drain",
    "days_mechanical_ventilation" = "Days of Mechanical Ventilation",
    "tracheostomy" = "Tracheostomy",
    "comfort_care_binary" = "Withdrawal of Life-Sustaining Therapy",
    "early_wlst" = "Early Withdrawal of Life-Sustaining Therapy",
    "dnr_binary" = "DNR Order"
  )

  # 2. Define the suffix pattern based on targets naming convention
  # Format: model_posterior_OUTCOME_SCENARIO_ADJUSTMENT
  # We assume we always want the "adjusted" models for Table 2
  suffix <- paste0("_", scenario, "_adjusted")

  # 3. Build the list
  selected_models <- list()

  for (outcome_col in names(target_map)) {
    # Reconstruct the target name
    target_name <- paste0("model_posterior_", outcome_col, suffix)

    # Validation
    if (!target_name %in% names(all_models)) {
      stop(paste(
        "Error in Table 2 creation: Could not find target",
        target_name,
        "in the combined list. Check if the model ran."
      ))
    }

    # Assign using the "Pretty Name" required by table_2_function
    pretty_name <- target_map[[outcome_col]]
    selected_models[[pretty_name]] <- all_models[[target_name]]
  }

  return(selected_models)
}
