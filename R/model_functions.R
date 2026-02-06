model_setup <- function(complexity = "fast") {
  if (complexity == "fast") {
    return(list(
      chains = 4,
      cores = 4,
      threads = NULL,
      iter = 5000,
      warmup = 1000,
      seed = 75832
    ))
  } else if (complexity == "complex") {
    return(list(
      chains = 4,
      cores = 4,
      threads = threading(4),
      iter = 5000,
      warmup = 1000,
      seed = 75832
    ))
  }
}

fit_laterality_model <- function(
  data,
  outcome_col,
  family,
  prior_scenario,
  adjustment_set = "adjusted",
  int_mean = 0,
  int_sd = 0.5,
  sample_prior = "no",
  settings
) {
  # -------------------------------------------------------------------------
  # Define Formula
  # -------------------------------------------------------------------------

  # Define the "Aggressive Care" list
  aggressive_outcomes <- c(
    "neurosurgery_evac",
    "evd",
    "tracheostomy",
    "days_mechanical_ventilation",
    "comfort_care_binary",
    "early_wlst",
    "dnr_binary"
  )

  # Construct the Left Hand Side
  # Handle special truncation syntax for ventilation
  lhs_string <- if_else(
    outcome_col == "days_mechanical_ventilation",
    paste0(outcome_col, " | trunc(lb = 1)"),
    outcome_col
  )

  # Construct the Right Hand Side

  # Scenario: Minimal
  base_formula <- as.formula(paste(
    lhs_string,
    "~ ich_laterality + (1 | study)"
  ))

  # Define the formula updates based on logic
  if (adjustment_set == "minimal") {
    final_formula <- base_formula
  } else if (adjustment_set == "adjusted") {
    # CHANGED: Used '*' to ensure ich_location main effect is included
    f <- update(
      base_formula,
      ~ . +
        ich_laterality * ich_location +
        age +
        gcs_baseline +
        ich_volume_baseline +
        ivh
    )

    # Conditionally add time-to-ED if it is NOT an aggressive outcome
    if (!outcome_col %in% aggressive_outcomes) {
      f <- update(f, ~ . + time_symptoms_to_ed)
    }

    final_formula <- f
  }

  # -------------------------------------------------------------------------
  # Define Priors
  # -------------------------------------------------------------------------

  # 1. Intercept Prior (From Function Arguments)
  my_priors <- c(
    set_prior(
      paste0("normal(", int_mean, ", ", int_sd, ")"),
      class = "Intercept"
    )
  )

  # 2. Coefficient Priors (Scenario Logic)
  if (prior_scenario == "flat") {
    my_priors <- c(my_priors, set_prior("normal(0, 5)", class = "b"))
  } else if (prior_scenario == "neutral") {
    my_priors <- c(my_priors, set_prior("normal(0, 0.5)", class = "b"))
  } else if (prior_scenario == "left") {
    my_priors <- c(
      my_priors,
      set_prior("normal(0, 0.5)", class = "b"),
      set_prior(
        "normal(-0.22, 0.175)",
        class = "b",
        coef = "ich_lateralityRight"
      )
    )
  } else if (prior_scenario == "right") {
    my_priors <- c(
      my_priors,
      set_prior("normal(0, 0.5)", class = "b"),
      set_prior(
        "normal(0.18, 0.175)",
        class = "b",
        coef = "ich_lateralityRight"
      )
    )
  }

  # -------------------------------------------------------------------------
  # Fit Model
  # -------------------------------------------------------------------------
  brm(
    formula = bf(final_formula),
    family = family,
    data = data,
    prior = my_priors,
    sample_prior = sample_prior,
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    backend = "cmdstanr",
    control = list(adapt_delta = 0.99)
  )
}
