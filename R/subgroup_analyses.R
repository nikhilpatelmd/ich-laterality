tar_load(m_posterior_neutral_neurosurgery)
tar_load(ich_aggressive)

# Function for stratification by ich_location
f_subgroup_by_location <- function(ich_aggressive) {

# Raw Numbers and Percentages
n <- ich_aggressive |>
  group_by(ich_laterality, ich_location) |>
  count() |>
  ungroup()

neurosurgery <- ich_aggressive |>
  group_by(ich_laterality, ich_location, neurosurgery_evac) |>
  count(name = "n_surgery") |>
  ungroup() |>
  filter(neurosurgery_evac == "Yes") |>
  select(ich_laterality, ich_location, n_surgery) |>
  left_join(n, by = c("ich_laterality", "ich_location")) |>
  mutate(
    pct = n_surgery / n
  ) |>
  pivot_wider(
    names_from = ich_laterality,
    values_from = c(n_surgery, pct, n)
  )

neurosurgery_formatted <- neurosurgery %>%
  mutate(
    Left = paste0(n_surgery_Left, " (", sprintf("%.1f", pct_Left * 100), "%)"),
    Right = paste0(n_surgery_Right, " (", sprintf("%.1f", pct_Right * 100), "%)")
  ) %>%
  select(ich_location, Left, Right)


# Stratified Subgroup Analysis----

# Settings
CHAINS <- 4
CORES <- 6
THREADS <- threading(1)
ITER <- 10000
WARMUP <- 5000
BAYES_SEED <- 2024 - 06 - 30

# Function to extract and summarize posterior draws
summarize_posterior <- function(model, term) {
posterior_draws <- posterior_samples(model)
posterior_draws %>%
  summarize(
    estimate = mean(exp(!!sym(term))),
    lower_ci = quantile(exp(!!sym(term)), 0.025),
    upper_ci = quantile(exp(!!sym(term)), 0.975),
    prob_greater_1 = mean(exp(!!sym(term)) > 1),
    prob_greater_1.2 = mean(exp(!!sym(term)) > 1.2),
    rope = mean(exp(!!sym(term)) >= 0.95 & exp(!!sym(term)) <= 1.05)
  )
}

location_stratified_results <- ich_aggressive %>%
group_by(ich_location) %>%
group_modify(~ {
  model <- brm(
    neurosurgery_evac ~ ich_laterality + age + gcs_baseline + ich_volume_baseline + ivh + (1 | study),
    family = bernoulli(link = "logit"),
    data =.,
    prior = c(
      set_prior("normal(-7, 0.35)", class = "Intercept"),
      set_prior("normal(0, 0.5)", class = "b")
    ),
    cores = CORES,
    chains = CHAINS,
    threads = THREADS,
    iter = ITER,
    warmup = WARMUP,
    seed = BAYES_SEED,
    backend = "cmdstanr" # Use cmdstanr backend
  )
  summarize_posterior(model, "b_ich_lateralityRight")
})

# Combine tables into subgroup table
joined_df <- neurosurgery_formatted %>%
  left_join(location_stratified_results, by = "ich_location") |>
  mutate(
    or = paste0(sprintf("%.2f", estimate), " (", sprintf("%.2f", lower_ci), " - ", sprintf("%.2f", upper_ci), ")")
  ) |>
  select(ich_location, Left, Right, or, prob_greater_1, prob_greater_1.2, rope)
  
return(joined_df)
  
}

# Function for stratification by study
f_subgroup_by_study <- function(ich_aggressive) {

  # Settings
  CHAINS <- 4
  CORES <- 6  # Adjust based on your CPU cores
  THREADS <- threading(1)  # Adjust based on your CPU threads
  ITER <- 10000
  WARMUP <- 5000
  BAYES_SEED <- 2024 - 06 - 30

  # Function to extract and summarize posterior draws
  summarize_posterior <- function(model, term) {
    posterior_draws <- posterior_samples(model)
    posterior_draws %>%
      summarize(
        estimate = mean(exp(!!sym(term))),
        lower_ci = quantile(exp(!!sym(term)), 0.025),
        upper_ci = quantile(exp(!!sym(term)), 0.975),
        prob_greater_1 = mean(exp(!!sym(term)) > 1),
        prob_greater_1.2 = mean(exp(!!sym(term)) > 1.2),
        rope = mean(exp(!!sym(term)) >= 0.95 & exp(!!sym(term)) <= 1.05)
      )
  }

  study_stratified_results <- ich_aggressive %>%
    group_by(study) %>%
    group_modify(~ {
      model <- brm(
        neurosurgery_evac ~ ich_laterality + age + gcs_baseline + ich_volume_baseline + ivh + ich_location,
        family = bernoulli(link = "logit"),
        data =.,
        prior = c(
          set_prior("normal(-7, 0.35)", class = "Intercept"),
          set_prior("normal(0, 0.5)", class = "b")
        ),
        cores = CORES,
        chains = CHAINS,
        threads = THREADS,
        iter = ITER,
        warmup = WARMUP,
        seed = BAYES_SEED,
        backend = "cmdstanr",
        refresh = 0
      )
      summarize_posterior(model, "b_ich_lateralityRight")
    })

  # Raw numbers and percentages (modified for study)
  n_study <- ich_aggressive %>%
    group_by(study, ich_laterality) %>%
    count() %>%
    ungroup()

  neurosurgery_study <- ich_aggressive %>%
    group_by(study, ich_laterality, neurosurgery_evac) %>%
    count(name = "n_surgery") %>%
    ungroup() %>%
    filter(neurosurgery_evac == "Yes") %>%
    select(study, ich_laterality, n_surgery) %>%
    left_join(n_study, by = c("study", "ich_laterality")) %>%
    mutate(
      pct = n_surgery / n
    ) %>%
    pivot_wider(
      names_from = ich_laterality,
      values_from = c(n_surgery, pct, n)
    )

  neurosurgery_study_formatted <- neurosurgery_study %>%
    mutate(
      Left = paste0(n_surgery_Left, " (", sprintf("%.1f", pct_Left * 100), "%)"),
      Right = paste0(n_surgery_Right, " (", sprintf("%.1f", pct_Right * 100), "%)")
    ) %>%
    select(study, Left, Right)

  # Combine tables into subgroup table
  joined_study_df <- neurosurgery_study_formatted %>%
    left_join(study_stratified_results, by = "study") %>%
    mutate(
      or = paste0(sprintf("%.2f", estimate), " (", sprintf("%.2f", lower_ci), " - ", sprintf("%.2f", upper_ci), ")")
    ) %>%
    select(study, Left, Right, or, prob_greater_1, prob_greater_1.2, rope)

  return(joined_study_df)
}