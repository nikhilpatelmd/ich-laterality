# R/sidecar_models.R

#' Standalone wrapper for Zero-Inflated Ventilation
#' Bypasses fit_laterality_model to ensure correct formula structure
fit_ventilation_zinb <- function(
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
  random_effect_str = "(1 | study)"
) {
  # 1. Build the Base Formula
  f_str <- paste(outcome_col, "~ ich_laterality")
  if (!is.null(random_effect_str)) {
    f_str <- paste(f_str, "+", random_effect_str)
  }

  # 2. Add confounders IF adjusted (Strictly excluding time_symptoms_to_ed)
  if (adjustment_set == "adjusted") {
    f_str <- paste(
      f_str,
      "+ ich_location + age + gcs_baseline + ich_volume_baseline + ivh + ich_laterality:ich_location"
    )
  }

  final_formula <- as.formula(f_str)

  # 3. Define Priors (Identical to main function)
  my_priors <- c(set_prior(
    paste0("normal(", int_mean, ", ", int_sd, ")"),
    class = "Intercept"
  ))

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

  # 4. Call brm() directly (No imputation loop, as this is complete-case aggressive care)
  brms::brm(
    formula = brms::bf(final_formula),
    family = family, # This will successfully receive the ZINB family
    data = data, # Retains the 0s perfectly
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
