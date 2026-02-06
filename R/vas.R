f_vas_90_zoib <- function(dat) {
  settings <- model_setup()

  # Scale to [0, 1]
  dat <- dat %>%
    mutate(
      euro_vas_90_scaled = euro_vas_90 / 100
    )

  # Define the formula
  f_zoib <- bf(
    # Part 1: The Mean of the continuous part (0 < y < 1)
    euro_vas_90_scaled ~ ich_laterality +
      age +
      ich_volume_baseline +
      gcs_baseline +
      ivh +
      ich_location,

    # Part 2: Precision (phi) - Corrected to be constant or predicted by covariates
    # (Previously you had this predicting itself)
    phi ~ ich_laterality,

    # Part 3: Zero-One Inflation (zoi) - Probability of being {0 or 1} vs {continuous}
    zoi ~ ich_laterality + age + gcs_baseline,

    # Part 4: Conditional One Inflation (coi) - Probability of being {1} given {0 or 1}
    # This specifically models the "Ceiling Effect"
    coi ~ ich_laterality
  )

  model <- brm(
    formula = f_zoib,
    family = zero_one_inflated_beta(),
    data = dat,
    # sample_prior = "only", # Uncomment for prior checks
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    init = 0 # Helps convergence for complex ZOIB models
  )

  return(model)
}
