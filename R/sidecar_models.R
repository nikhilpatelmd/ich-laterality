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

  # 3. Define Priors (Now with shape and zi regularization)
  my_priors <- c(
    set_prior(
      paste0("normal(", int_mean, ", ", int_sd, ")"),
      class = "Intercept"
    ),
    set_prior("gamma(2, 0.1)", class = "shape"), # Mean shape = 20; prevents near-zero draws
    set_prior("beta(1, 1)", class = "zi")
  )

  if (prior_scenario == "flat") {
    # Still wider than neutral, but bounded to prevent exponential blowup.
    # normal(0, 0.2) on the log scale: 2 SD = 0.4 → exp(0.4) ≈ 1.5x per unit.
    my_priors <- c(my_priors, set_prior("normal(0, 0.2)", class = "b"))
  } else if (prior_scenario == "neutral") {
    my_priors <- c(my_priors, set_prior("normal(0, 0.05)", class = "b"))
  } else if (prior_scenario == "left") {
    my_priors <- c(
      my_priors,
      set_prior("normal(0, 0.05)", class = "b"),
      set_prior(
        "normal(-0.22, 0.175)",
        class = "b",
        coef = "ich_lateralityRight"
      )
    )
  } else if (prior_scenario == "right") {
    my_priors <- c(
      my_priors,
      set_prior("normal(0, 0.05)", class = "b"),
      set_prior(
        "normal(0.18, 0.175)",
        class = "b",
        coef = "ich_lateralityRight"
      )
    )
  }

  # 4. Call brm() directly
  brms::brm(
    formula = brms::bf(final_formula),
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
