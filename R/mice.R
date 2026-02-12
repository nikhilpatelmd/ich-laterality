library(mice)
library(ggplot2)
library(patchwork)
library(dplyr)
library(stringr)
library(tibble)

f_imputed <- function(data, n_imputes = 20, seed = 1) {
  # 1. Select Variables for Imputation
  fixed_vars <- c(
    "neurosurgery_evac",
    "ich_laterality",
    "ich_location",
    "age",
    "ivh",
    "gcs_baseline",
    "ich_volume_baseline",
    "study",
    "htn",
    "evd",
    "time_symptoms_to_ed",
    # --- Aggressive Care Outcomes ---
    "comfort_care_binary",
    "early_wlst",
    "dnr_binary",
    "tracheostomy",
    "days_mechanical_ventilation" # Included here to PRESERVE the column
  )

  # Mirror the logic from missing data analysis
  imp_data <- data |>
    select(
      any_of(fixed_vars),
      starts_with(c("mrs", "euro")),
      -ends_with(c("180", "365")),
      -ends_with(c("01", "02", "03", "04"))
    ) |>
    # Ensure categorical predictors are factors
    mutate(across(
      c(
        neurosurgery_evac,
        ivh,
        htn,
        evd,
        ich_location,
        ich_laterality,
        # Binary outcomes only (exclude days_mechanical_ventilation)
        comfort_care_binary,
        early_wlst,
        dnr_binary,
        tracheostomy
      ),
      as.factor
    ))

  # 2. Setup MICE (Dry Run)
  ini <- mice(imp_data, maxit = 0)

  # 3. Define Methods
  meth <- tibble(variable = names(ini$method)) |>
    mutate(
      method = case_when(
        # A. Do not impute Fixed Variables or Structural Missingness
        variable == "study" ~ "",

        # EXPLICITLY SKIP days_mechanical_ventilation
        # (It falls to TRUE ~ "" anyway, but being explicit is safer for future reads)
        variable == "days_mechanical_ventilation" ~ "",

        # B. Dynamic Outcomes (EuroQOL, mRS)
        str_starts(variable, "euro|mrs") ~ "pmm",

        # C. Continuous / Ordinal Predictors
        variable %in%
          c(
            "age",
            "gcs_baseline",
            "ich_volume_baseline",
            "time_symptoms_to_ed"
          ) ~ "pmm",

        # D. Binary Predictors
        variable %in% c("neurosurgery_evac", "ivh", "htn", "evd") ~ "logreg",

        # E. Polytomous Predictors
        variable %in% c("ich_location", "ich_laterality") ~ "polyreg",

        # F. Safety Fallback (Empty string = no imputation)
        # This will catch comfort_care_binary, early_wlst, etc.
        TRUE ~ ""
      )
    ) |>
    deframe()

  # 4. Run Imputation
  imp <- mice(
    imp_data,
    method = meth,
    m = n_imputes,
    maxit = 20,
    seed = seed,
    print = FALSE
  )

  return(imp)
}

f_plot_imputations_detailed <- function(mids_object) {
  # (Same plotting code as before)
  long_data <- complete(mids_object, action = "long", include = TRUE) |>
    mutate(
      Type = ifelse(.imp == 0, "Observed", "Imputed"),
      Alpha = ifelse(.imp == 0, 1, 0.1)
    )

  p_bivariate <- ggplot(
    long_data,
    aes(x = ich_volume_baseline, y = gcs_baseline)
  ) +
    geom_jitter(
      data = filter(long_data, .imp > 0),
      color = "firebrick",
      alpha = 0.05,
      width = 0,
      height = 0.2
    ) +
    geom_point(
      data = filter(long_data, .imp == 0),
      color = "steelblue",
      alpha = 0.8
    ) +
    geom_smooth(
      aes(group = .imp, color = Type),
      method = "lm",
      se = FALSE,
      size = 0.5,
      alpha = 0.5
    ) +
    scale_color_manual(
      values = c("Imputed" = "firebrick", "Observed" = "steelblue")
    ) +
    labs(
      title = "Bivariate Consistency: Volume vs GCS",
      subtitle = "Red lines (Imputations) should bundle around Blue line (Observed)",
      x = "ICH Volume",
      y = "GCS Baseline"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  p_cat <- long_data |>
    group_by(.imp, ich_location) |>
    summarise(count = n(), .groups = "drop") |>
    mutate(prop = count / sum(count), .by = .imp) |>
    mutate(Type = ifelse(.imp == 0, "Observed", "Imputed")) |>
    ggplot(aes(x = ich_location, y = prop, color = Type, group = .imp)) +
    geom_line(alpha = 0.3) +
    geom_line(data = . %>% filter(.imp == 0), color = "steelblue", size = 1.5) +
    scale_color_manual(
      values = c("Imputed" = "firebrick", "Observed" = "steelblue")
    ) +
    labs(
      title = "Categorical Distribution: ICH Location",
      y = "Proportion",
      x = NULL
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  p_outcome <- ggplot(long_data, aes(x = mrs_90, group = .imp, color = Type)) +
    geom_density(adjust = 2) +
    scale_color_manual(
      values = c("Imputed" = "firebrick", "Observed" = "steelblue")
    ) +
    labs(title = "Outcome Distribution: mRS 90") +
    theme_minimal()

  layout <- (p_bivariate + p_outcome) / p_cat
  return(layout)
}
