fit_vas_zoib <- function(
  data,
  use_imputation,
  prior_scenario,
  adjustment_set,
  settings,
  random_effect_str = "(1 | study)",
  sample_prior = "no"
) {
  # 1. Handle imputed data vs. raw data and scale the outcome
  if (use_imputation && is.character(data)) {
    imp_obj <- readRDS(data)

    # Extract the list of dataframes from the mids object
    if (inherits(imp_obj, "mids")) {
      dat_list <- mice::complete(imp_obj, action = "all")
    } else {
      dat_list <- imp_obj
    }

    # Scale euro_vas_90 across all imputed datasets using purrr/dplyr
    dat <- purrr::map(
      dat_list,
      ~ .x |>
        dplyr::mutate(euro_vas_90_scaled = euro_vas_90 / 100)
    )
  } else {
    # Scale single dataset
    dat <- data |>
      dplyr::mutate(euro_vas_90_scaled = euro_vas_90 / 100)
  }

  # 2. Define formula based on adjustment_set
  base_vars <- "ich_laterality"
  if (adjustment_set == "adjusted") {
    covariates <- "age + ich_volume_baseline + gcs_baseline + ivh + ich_location"
    f_str <- paste("euro_vas_90_scaled ~", base_vars, "+", covariates)
    if (!is.null(random_effect_str)) {
      f_str <- paste(f_str, "+", random_effect_str)
    }
  } else {
    f_str <- paste("euro_vas_90_scaled ~", base_vars)
  }

  # Build the ZOIB formula
  f_zoib <- brms::bf(
    as.formula(f_str),
    phi ~ ich_laterality,
    zoi ~ ich_laterality + age + gcs_baseline,
    coi ~ ich_laterality
  )

  # 3. Set priors based on prior_scenario (neutral, left, right, flat)
  if (prior_scenario == "flat") {
    my_priors <- brms::set_prior("normal(0, 5)", class = "b")
  } else if (prior_scenario == "neutral") {
    my_priors <- brms::set_prior("normal(0, 0.5)", class = "b")
  } else if (prior_scenario == "left") {
    my_priors <- c(
      brms::set_prior("normal(0, 0.5)", class = "b"),
      brms::set_prior(
        "normal(-0.22, 0.175)",
        class = "b",
        coef = "ich_lateralityRight"
      )
    )
  } else if (prior_scenario == "right") {
    my_priors <- c(
      brms::set_prior("normal(0, 0.5)", class = "b"),
      brms::set_prior(
        "normal(0.18, 0.175)",
        class = "b",
        coef = "ich_lateralityRight"
      )
    )
  }

  # 4. Fit model (switching based on imputation status)
  if (use_imputation) {
    model <- brms::brm_multiple(
      formula = f_zoib,
      family = brms::zero_one_inflated_beta(),
      data = dat,
      prior = my_priors,
      sample_prior = sample_prior,
      chains = 1, # brm_multiple handles chains differently across list items
      cores = settings$cores,
      warmup = settings$warmup,
      iter = settings$iter,
      seed = settings$seed,
      backend = settings$backend,
      control = settings$control,
      init = 0
    )
  } else {
    model <- brms::brm(
      formula = f_zoib,
      family = brms::zero_one_inflated_beta(),
      data = dat,
      prior = my_priors,
      sample_prior = sample_prior,
      cores = settings$cores,
      chains = settings$chains,
      threads = settings$threads,
      warmup = settings$warmup,
      iter = settings$iter,
      seed = settings$seed,
      backend = settings$backend,
      control = settings$control,
      init = 0
    )
  }

  return(model)
}


fit_vas_zoib_prior_only <- function(
  data,
  use_imputation,
  prior_scenario,
  adjustment_set,
  settings,
  random_effect_str = "(1 | study)",
  sample_prior = "only"
) {
  # 1. Scale single dataset (priors don't need imputed data)
  dat <- data |>
    dplyr::mutate(euro_vas_90_scaled = euro_vas_90 / 100)

  # 2. Define formula based on adjustment_set
  base_vars <- "ich_laterality"
  if (adjustment_set == "adjusted") {
    covariates <- "age + ich_volume_baseline + gcs_baseline + ivh + ich_location"
    f_str <- paste("euro_vas_90_scaled ~", base_vars, "+", covariates)
    if (!is.null(random_effect_str)) {
      f_str <- paste(f_str, "+", random_effect_str)
    }
  } else {
    f_str <- paste("euro_vas_90_scaled ~", base_vars)
  }

  # Build the ZOIB formula
  f_zoib <- brms::bf(
    as.formula(f_str),
    phi ~ ich_laterality,
    zoi ~ ich_laterality + age + gcs_baseline,
    coi ~ ich_laterality
  )

  # 3. Set priors
  if (prior_scenario == "flat") {
    my_priors <- c(brms::set_prior("normal(0, 5)", class = "b"))
  } else if (prior_scenario == "neutral") {
    my_priors <- c(brms::set_prior("normal(0, 0.5)", class = "b"))
  } else if (prior_scenario == "left") {
    my_priors <- c(
      brms::set_prior("normal(0, 0.5)", class = "b"),
      brms::set_prior(
        "normal(-0.22, 0.175)",
        class = "b",
        coef = "ich_lateralityRight"
      )
    )
  } else if (prior_scenario == "right") {
    my_priors <- c(
      brms::set_prior("normal(0, 0.5)", class = "b"),
      brms::set_prior(
        "normal(0.18, 0.175)",
        class = "b",
        coef = "ich_lateralityRight"
      )
    )
  }

  # Add the proper priors for the distributional parameters to prevent the crash
  my_priors <- c(
    my_priors,
    brms::set_prior("normal(0, 5)", class = "b", dpar = "phi"),
    brms::set_prior("normal(0, 1.5)", class = "b", dpar = "zoi"),
    brms::set_prior("normal(0, 1.5)", class = "b", dpar = "coi")
  )

  # 4. Fit model (Forced to complete case and sample_prior)
  model <- brms::brm(
    formula = f_zoib,
    family = brms::zero_one_inflated_beta(),
    data = dat,
    prior = my_priors,
    sample_prior = sample_prior,
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    backend = settings$backend,
    control = settings$control,
    init = 0
  )

  return(model)
}