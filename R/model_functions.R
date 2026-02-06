fit_laterality_model <- function(
  data,
  outcome_col,
  family,
  prior_scenario,
  sample_prior = "no",
  settings
) {
  # Base predictors
  predictors <- "ich_laterality * ich_location + age + gcs_baseline + ich_volume_baseline + ivh + (1 | study) "

  # Mechanical ventilation Specification
  if (outcome_col == "days_mechanical_ventilation") {
    form_string <- paste0(outcome_col, " | trunc(lb = 1) ~ ", predictors)
  } else {
    form_string <- paste0(outcome_col, " ~ ", predictors)
  }

  model_formula <- as.formula(form_string)

  # Prior specification
  base_priors <- c()

  # Intercept Priors (vary by outcome AND scenario)
  # You might want to pass specific intercept means as an argument,
  # but here is a simplified logic block based on your code:

  intercept_mean <- 0
  intercept_sd <- 0.5

  if (prior_scenario == "flat") {
    intercept_mean <- 0
    intercept_sd <- 1
  } else {
    # Logic derived from your existing code
    if (outcome_col == "neurosurgery_evac") {
      intercept_mean <- -7
    }
    if (outcome_col == "tracheostomy") {
      intercept_mean <- -15
    }
    if (outcome_col == "days_mechanical_ventilation") {
      intercept_mean <- 2
      intercept_sd <- 1
    }
    if (outcome_col == "early_wlst") {
      intercept_mean <- -20
    }
    if (outcome_col %in% c("comfort_care_binary", "dnr_binary")) {
      intercept_mean <- -15
    }
  }

  base_priors <- c(
    base_priors,
    set_prior(
      paste0("normal(", intercept_mean, ", ", intercept_sd, ")"),
      class = "Intercept"
    )
  )

  # Coefficient Priors (The 'b' class)
  if (prior_scenario == "flat") {
    base_priors <- c(base_priors, set_prior("normal(0, 5)", class = "b"))
  } else if (prior_scenario == "neutral") {
    base_priors <- c(base_priors, set_prior("normal(0, 0.5)", class = "b"))
  } else if (prior_scenario == "left") {
    # Left implies bias: usually specific coefs.
    base_priors <- c(
      base_priors,
      set_prior(
        "normal(0, 0.5)",
        class = "b"
      ),
      set_prior(
        "normal(-0.22, 0.175)",
        class = "b",
        coef = "ich_lateralityRight"
      )
    )
  } else if (prior_scenario == "right") {
    base_priors <- c(
      base_priors,
      set_prior(
        "normal(0, 0.5)",
        class = "b"
      ),
      set_prior(
        "normal(0.18, 0.175)",
        class = "b",
        coef = "ich_lateralityRight"
      )
    )
  }

  # Run Model
  brm(
    formula = model_formula,
    data = data,
    family = family,
    prior = base_priors,
    sample_prior = sample_prior,
    chains = settings$chains,
    cores = settings$cores,
    threads = settings$threads,
    iter = settings$iter,
    warmup = settings$warmup,
    seed = settings$seed,
    backend = "cmdstanr"
  )
}
