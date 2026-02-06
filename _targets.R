suppressPackageStartupMessages({
  library(targets)
  library(tarchetypes)
  library(tibble)
  library(dplyr)
  library(brms)
  library(quarto)
  library(here)
  library(future)
  library(future.callr)
})

# 1. PARALLEL PLAN --------------------------------------------------------
plan(callr, workers = 4)

# General pipeline settings ----
options(brms.backend = "cmdstanr")
options(tidyverse.quiet = TRUE, dplyr.summarise.inform = FALSE)

# R functions ----
source("R/packages.R")
source("R/data_cleaning.R")
source("R/mice.R")
source("R/dags.R")
source("R/diagnostics.R")
source("R/model_functions.R")
source("R/predictive_checks.R")
source("R/predictive_checks_ventilation.R")
source("R/posterior_diagnostics.R")
source("R/table1.R")
source("R/table2.R")
source("R/figure_1.R")
source("R/subgroup_analyses.R")
source("R/table4.R")
source("R/figures.R")
source("R/mrs_figures.R")
source("R/euro_figures.R")
source("R/vas.R")
source("R/imputed_data.R")
source("R/sensitivity.R")

# 2. DEFINE GRIDS ---------------------------------------------------------

# Aggressive Care Outcomes
aggressive_grid <- tibble::tribble(
  ~outcome_col                  , ~family                          , ~int_mean , ~int_sd , ~complexity ,
  "neurosurgery_evac"           , quote(bernoulli(link = "logit")) ,        -7 , 0.35    , "fast"      ,
  "evd"                         , quote(bernoulli(link = "logit")) ,         0 , 0.5     , "fast"      ,
  "days_mechanical_ventilation" , quote(negbinomial(link = "log")) ,         0 , 0.5     , "complex"   ,
  "dnr_binary"                  , quote(bernoulli(link = "logit")) ,        -5 , 0.5     , "fast"      ,
  "comfort_care_binary"         , quote(bernoulli(link = "logit")) ,        -5 , 0.5     , "fast"      ,
  "early_wlst"                  , quote(bernoulli(link = "logit")) ,         0 , 0.5     , "fast"      ,
  "tracheostomy"                , quote(bernoulli(link = "logit")) ,       -15 , 0.5     , "fast"
)

# Functional Outcomes
functional_outcomes <- tidyr::expand_grid(
  domain = c(
    "mrs",
    "euro_mobility",
    "euro_selfcare",
    "euro_usual",
    "euro_pain",
    "euro_anxiety"
  ),
  time = c("90", "180", "365")
) %>%
  mutate(outcome_col = paste0(domain, "_", time)) %>%
  pull(outcome_col)

functional_grid <- tibble(
  outcome_col = functional_outcomes,
  family = rep(
    list(quote(cumulative(link = "logit"))),
    length(functional_outcomes)
  ),
  int_mean = -2.2,
  int_sd = 0.5,
  complexity = "complex"
)

# Combine
base_grid <- aggressive_grid
# base_grid <- bind_rows(aggressive_grid, functional_grid)

# Cross with Scenarios
complete_grid <- tidyr::crossing(
  base_grid,
  prior_scenario = c("neutral", "left", "right", "flat"),
  adjustment_set = c("minimal", "adjusted")
)

# SPLIT THE GRID
grid_fast <- complete_grid |> filter(complexity == "fast")
grid_complex <- complete_grid |> filter(complexity == "complex")

# Table Scenarios
table_scenarios <- tibble(scenario = c("neutral", "left", "right", "flat"))

# 3. DEFINE MAPS (Outside of tar_plan) ------------------------------------
# We assign these to variables so we can reference them later in tar_combine

# Map 1: Fast Models
map_fast <- tar_map(
  values = grid_fast,
  names = c("outcome_col", "prior_scenario", "adjustment_set"),

  # Posterior
  tar_target(
    model_posterior,
    fit_laterality_model(
      data = ich_aggressive,
      outcome_col = outcome_col,
      family = family,
      prior_scenario = prior_scenario,
      adjustment_set = adjustment_set,
      int_mean = int_mean,
      int_sd = int_sd,
      sample_prior = "no",
      settings = model_setup("fast")
    ),
    deployment = "worker"
  ),

  # Prior Check
  tar_target(
    model_prior,
    fit_laterality_model(
      data = ich_aggressive,
      outcome_col = outcome_col,
      family = family,
      prior_scenario = prior_scenario,
      adjustment_set = adjustment_set,
      sample_prior = "only",
      settings = model_setup("fast")
    ),
    deployment = "worker"
  )
)

# Map 2: Complex Models
map_complex <- tar_map(
  values = grid_complex,
  names = c("outcome_col", "prior_scenario", "adjustment_set"),

  # Posterior
  tar_target(
    model_posterior,
    fit_laterality_model(
      data = ich_aggressive,
      outcome_col = outcome_col,
      family = family,
      prior_scenario = prior_scenario,
      adjustment_set = adjustment_set,
      int_mean = int_mean,
      int_sd = int_sd,
      sample_prior = "no",
      settings = model_setup("complex")
    ),
    deployment = "main"
  ),

  # Prior Check
  tar_target(
    model_prior,
    fit_laterality_model(
      data = ich_aggressive,
      outcome_col = outcome_col,
      family = family,
      prior_scenario = prior_scenario,
      adjustment_set = adjustment_set,
      sample_prior = "only",
      settings = model_setup("fast")
    ),
    deployment = "worker"
  )
)

# 4. FILTER FOR COMBINATION -----------------------------------------------
# We only want to combine the *posterior* models, not the priors.
# We search the lists of targets we just created for names starting with "model_posterior"

posteriors_fast <- map_fast[grep("^model_posterior", names(map_fast))]
posteriors_complex <- map_complex[grep("^model_posterior", names(map_complex))]

# 5. PIPELINE -------------------------------------------------------------
tar_plan(
  ## Data & Setup ----
  tar_file_read(imported_data, "data/raw_data/all.rds", read_rds(!!.x)),
  left_fill = "#ce4951",
  right_fill = "#476170",
  theme_ich = theme_ich(),

  selected_data = select_variables(imported_data),
  ich_all = filter_variables(selected_data),
  ich_aggressive = ich_all |>
    filter(study == "ERICH" | study == "ATACH-2") |>
    droplevels(),
  ich_imputed = f_imputed(ich_aggressive),

  dag_neurosurgery = f_neurosurgery_dag(x),
  dag_outcomes = outcomes_dag_function(x),
  settings = model_setup(),

  # Include the Model Maps
  map_fast,
  map_complex,

  # Combine Results
  # We pass the pre-filtered lists of targets
  tar_combine(
    all_posterior_models,
    posteriors_fast,
    posteriors_complex
  ),

  # Generate Tables
  tar_map(
    values = table_scenarios,

    tar_target(
      table_2,
      table_2_function(
        x = ich_aggressive,
        models = subset_models_for_table2(all_posterior_models, scenario)
      )
    )
  )
)
