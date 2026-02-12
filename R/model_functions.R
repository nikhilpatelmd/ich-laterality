# R/model_functions.R

model_setup <- function(complexity = "complex") {
  if (complexity == "complex") {
    # --- MANUSCRIPT PRODUCTION MODE ---
    # Optimized for AMD Ryzen AI 9 (24 threads)
    # Strategy: Run 4 chains on 4 separate cores.
    # Disable within-chain threading (it adds overhead for N < 10k).
    list(
      chains = 4,
      cores = 1,
      threads = NULL,
      iter = 4000,
      warmup = 2000,
      control = list(adapt_delta = 0.99),
      seed = 20240630,
      backend = "cmdstanr"
    )
  } else {
    # --- FAST / DEBUG MODE ---
    # Strategy: Minimum viability to check pipeline mechanics.
    list(
      chains = 1,
      cores = 1,
      threads = NULL,
      iter = 100,
      warmup = 50,
      control = list(adapt_delta = 0.80),
      seed = 20240630,
      backend = "cmdstanr"
    )
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
  settings,
  use_imputation = FALSE,
  random_effect_str = "(1 | study)",
  interaction_var = NULL
) {
  # --- NEW: LOAD DATA FROM FILE IF NEEDED ---
  # This prevents passing massive objects through parallel workers
  if (is.character(data) && length(data) == 1 && file.exists(data)) {
    data <- readRDS(data)
  }
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
  lhs_string <- if_else(
    outcome_col == "days_mechanical_ventilation",
    paste0(outcome_col, " | trunc(lb = 1)"),
    outcome_col
  )

  # Construct the Right Hand Side
  # We dynamically paste the random effect string provided in arguments
  re_term <- if (!is.null(random_effect_str)) {
    paste("+", random_effect_str)
  } else {
    ""
  }

  # Base predictor term:
  # If interaction_var is present, we multiply laterality by it.
  # Otherwise, we just use laterality.
  predictor_term <- if (!is.null(interaction_var)) {
    paste("ich_laterality *", interaction_var)
  } else {
    "ich_laterality"
  }

  base_formula <- as.formula(paste(
    lhs_string,
    "~",
    predictor_term,
    re_term
  ))

  if (adjustment_set == "minimal") {
    final_formula <- base_formula
  } else if (adjustment_set == "adjusted") {
    # Start with base formula
    f <- base_formula

    # Add confounders
    # Note: We check if the interaction variable is already in the confounder list
    # to avoid duplication, though update() usually handles it.
    confounders <- c(
      "ich_location",
      "age",
      "gcs_baseline",
      "ich_volume_baseline",
      "ivh"
    )

    # If the interaction variable is one of our standard adjusters, remove it
    # from the "add" list because it's already in the base formula as an interaction.
    if (!is.null(interaction_var)) {
      confounders <- setdiff(confounders, interaction_var)
    }

    # Construct the update formula string
    update_str <- paste("~ . +", paste(confounders, collapse = " + "))

    # Only add interaction between laterality and location IF we are NOT
    # already testing location as the main interaction variable.
    if (is.null(interaction_var) || interaction_var != "ich_location") {
      update_str <- paste(update_str, "+ ich_laterality:ich_location")
    }

    f <- update(f, as.formula(update_str))

    if (!outcome_col %in% aggressive_outcomes) {
      f <- update(f, ~ . + time_symptoms_to_ed)
    }

    final_formula <- f
  }

  # -------------------------------------------------------------------------
  # Define Priors
  # -------------------------------------------------------------------------
  my_priors <- c(
    set_prior(
      paste0("normal(", int_mean, ", ", int_sd, ")"),
      class = "Intercept"
    )
  )

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

  if (use_imputation) {
    brm_multiple(
      formula = bf(final_formula),
      family = family,
      data = data,
      prior = my_priors,
      sample_prior = sample_prior,
      chains = 1, # brm_multiple runs 1 chain per imputed dataset
      cores = settings$cores,
      warmup = settings$warmup,
      iter = settings$iter,
      seed = settings$seed,
      backend = "cmdstanr",
      control = settings$control
    )
  } else {
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
      control = settings$control
    )
  }
}
