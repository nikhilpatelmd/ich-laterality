library(mice)
library(ggplot2)
library(patchwork)
library(dplyr)
library(stringr)
library(tibble)

f_imputed <- function(data, n_imputes = 100, seed = 1) {
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
    "time_symptoms_to_ed"
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
    # Note: We do NOT use any_of() here because if these are missing,
    # we WANT the pipeline to error (fail-fast).
    mutate(across(
      c(neurosurgery_evac, ivh, htn, evd, ich_location, ich_laterality),
      as.factor
    ))

  # 2. Setup MICE (Dry Run)
  ini <- mice(imp_data, maxit = 0)

  # 3. Define Methods
  # Create a tibble mapping Variable Name -> Method
  meth <- tibble(variable = names(ini$method)) |>
    mutate(
      method = case_when(
        # A. Do not impute Fixed Variables
        variable == "study" ~ "",

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
        TRUE ~ ""
      )
    ) |>
    # Convert back to the named vector 'mice' expects
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
  # 1. TRACE PLOTS (Convergence) --------------------------------------------
  # We use the base plot() from mice because it handles the chains automatically.
  # We save it to a file or return it as a grob if needed, but for targets,
  # it's often easier to just rely on the density/scatter plots for the report.
  # (You should run plot(mids_object) interactively to check for "caterpillars")

  # 2. DATA PREPARATION -----------------------------------------------------
  # Convert MICE object to long format for ggplot
  # .imp == 0 is original data, .imp > 0 is imputed
  long_data <- complete(mids_object, action = "long", include = TRUE) |>
    mutate(
      Type = ifelse(.imp == 0, "Observed", "Imputed"),
      # Make points smaller/transparent for imputed to see density
      Alpha = ifelse(.imp == 0, 1, 0.1)
    )

  # 3. BIVARIATE CHECK (Volume vs GCS) --------------------------------------
  # Does the relationship between Severity (Volume) and Status (GCS) hold?
  # We expect a negative correlation (High Vol -> Low GCS)

  p_bivariate <- ggplot(
    long_data,
    aes(x = ich_volume_baseline, y = gcs_baseline)
  ) +
    # Plot imputed points first (in background, red)
    geom_jitter(
      data = filter(long_data, .imp > 0),
      color = "firebrick",
      alpha = 0.05,
      width = 0,
      height = 0.2
    ) +
    # Plot observed points on top (blue)
    geom_point(
      data = filter(long_data, .imp == 0),
      color = "steelblue",
      alpha = 0.8
    ) +
    # Add trend lines to compare slopes
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

  # 4. CATEGORICAL CHECK (ICH Location) -------------------------------------
  # Did we accidentally over-impute "Lobar" hemorrhages?

  p_cat <- long_data |>
    group_by(.imp, ich_location) |>
    summarise(count = n(), .groups = "drop") |>
    mutate(prop = count / sum(count), .by = .imp) |>
    mutate(Type = ifelse(.imp == 0, "Observed", "Imputed")) |>
    ggplot(aes(x = ich_location, y = prop, color = Type, group = .imp)) +
    # Plot lines for each imputation
    geom_line(alpha = 0.3) +
    # Highlight observed data
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

  # 5. UNIVARIATE DENSITY (Outcomes) ----------------------------------------
  # Checking mRS 90 distribution
  p_outcome <- ggplot(long_data, aes(x = mrs_90, group = .imp, color = Type)) +
    geom_density(adjust = 2) + # Smooth it out a bit
    scale_color_manual(
      values = c("Imputed" = "firebrick", "Observed" = "steelblue")
    ) +
    labs(title = "Outcome Distribution: mRS 90") +
    theme_minimal()

  # Combine
  layout <- (p_bivariate + p_outcome) / p_cat

  return(layout)
}
