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
  library(tidyr)
})

# 1. PARALLEL PLAN --------------------------------------------------------
plan(callr, workers = 5)

# General pipeline settings ----
options(brms.backend = "cmdstanr")
options(tidyverse.quiet = TRUE, dplyr.summarise.inform = FALSE)

# R functions ----
source("R/packages.R")
source("R/data_cleaning.R")
source("R/mice.R")
source("R/dags.R")
source("R/missing_data.R")
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
) |>
  mutate(outcome_col = paste0(domain, "_", time)) |>
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

# --- TRACK A: MAIN ANALYSIS (Imputed) ---
map_main_fast <- tar_map(
  values = grid_fast,
  names = c("outcome_col", "prior_scenario", "adjustment_set"),
  unlist = FALSE,

  tar_target(
    model_main,
    list(
      fit_laterality_model(
        data = ich_imputed,
        use_imputation = TRUE,
        outcome_col = outcome_col,
        family = family,
        prior_scenario = prior_scenario,
        adjustment_set = adjustment_set,
        int_mean = int_mean,
        int_sd = int_sd,
        sample_prior = "no",
        settings = model_setup("fast"),
        random_effect_str = "(1 | study)" # Default behavior
      )
    ),
    deployment = "worker"
  )
)

map_main_complex <- tar_map(
  values = grid_complex,
  names = c("outcome_col", "prior_scenario", "adjustment_set"),
  unlist = FALSE,

  tar_target(
    model_main,
    list(
      fit_laterality_model(
        data = ich_imputed,
        use_imputation = TRUE,
        outcome_col = outcome_col,
        family = family,
        prior_scenario = prior_scenario,
        adjustment_set = adjustment_set,
        int_mean = int_mean,
        int_sd = int_sd,
        sample_prior = "no",
        settings = model_setup("complex"),
        random_effect_str = "(1 | study)"
      )
    ),
    deployment = "main"
  )
)

# --- TRACK B: SENSITIVITY ANALYSIS (Complete Case) ---
map_sens_fast <- tar_map(
  values = grid_fast,
  names = c("outcome_col", "prior_scenario", "adjustment_set"),
  unlist = FALSE,

  tar_target(
    model_sens,
    list(
      fit_laterality_model(
        data = ich_aggressive,
        use_imputation = FALSE,
        outcome_col = outcome_col,
        family = family,
        prior_scenario = prior_scenario,
        adjustment_set = adjustment_set,
        int_mean = int_mean,
        int_sd = int_sd,
        sample_prior = "no",
        settings = model_setup("fast"),
        random_effect_str = "(1 | study)"
      )
    ),
    deployment = "worker"
  )
)

map_sens_complex <- tar_map(
  values = grid_complex,
  names = c("outcome_col", "prior_scenario", "adjustment_set"),
  unlist = FALSE,

  tar_target(
    model_sens,
    list(
      fit_laterality_model(
        data = ich_aggressive,
        use_imputation = FALSE,
        outcome_col = outcome_col,
        family = family,
        prior_scenario = prior_scenario,
        adjustment_set = adjustment_set,
        int_mean = int_mean,
        int_sd = int_sd,
        sample_prior = "no",
        settings = model_setup("complex"),
        random_effect_str = "(1 | study)"
      )
    ),
    deployment = "main"
  )
)

# --- TRACK C: SITE CLUSTERING SENSITIVITY (ATACH-2 Only) ---
grid_atach_sens <- aggressive_grid |>
  filter(outcome_col == "neurosurgery_evac") |> # Primary outcome only
  tidyr::crossing(
    prior_scenario = "neutral",
    adjustment_set = "adjusted"
  )

map_atach_sens <- tar_map(
  values = grid_atach_sens,
  names = "outcome_col",
  unlist = FALSE,

  # Model A: ATACH-2 Only, NO Random Effects (Base)
  tar_target(
    model_atach_base,
    list(
      fit_laterality_model(
        data = ich_aggressive |> filter(study == "ATACH-2"),
        outcome_col = outcome_col,
        family = family,
        prior_scenario = prior_scenario,
        adjustment_set = adjustment_set,
        int_mean = int_mean,
        int_sd = int_sd,
        sample_prior = "no",
        settings = model_setup("fast"),
        use_imputation = FALSE,
        random_effect_str = NULL
      )
    ),
    deployment = "worker"
  ),

  # Model B: ATACH-2 Only, WITH Site Random Effects
  tar_target(
    model_atach_site,
    list(
      fit_laterality_model(
        data = ich_aggressive |> filter(study == "ATACH-2"),
        outcome_col = outcome_col,
        family = family,
        prior_scenario = prior_scenario,
        adjustment_set = adjustment_set,
        int_mean = int_mean,
        int_sd = int_sd,
        sample_prior = "no",
        settings = model_setup("fast"),
        use_imputation = FALSE,
        random_effect_str = "(1 | site_id)" # <--- ENSURE COL NAME MATCHES
      )
    ),
    deployment = "worker"
  )
)

# --- TRACK D: INTERACTION CHECKS (Formal Tests) ---
# Testing if Laterality effect varies by Location or Study
grid_interactions <- tibble(
  outcome_col = "neurosurgery_evac", # Primary outcome only
  interaction_var = c("ich_location", "study"),
  family = rep(list(quote(bernoulli(link = "logit"))), 2),
  prior_scenario = "neutral",
  adjustment_set = "adjusted"
)

map_interactions <- tar_map(
  values = grid_interactions,
  names = "interaction_var",
  unlist = FALSE,

  tar_target(
    model_interaction,
    list(
      fit_laterality_model(
        data = ich_aggressive, # Use raw data for simplicity
        use_imputation = FALSE,
        outcome_col = outcome_col,
        family = family,
        prior_scenario = prior_scenario,
        adjustment_set = adjustment_set,
        interaction_var = interaction_var, # Trigger interaction term
        settings = model_setup("fast")
      )
    ),
    deployment = "worker"
  )
)


# 4. FILTER FOR COMBINATION -----------------------------------------------

# Gather Main Models
posteriors_main <- c(
  map_main_fast[grep("^model_main", names(map_main_fast))],
  map_main_complex[grep("^model_main", names(map_main_complex))]
)

# Gather Sensitivity Models
posteriors_sens <- c(
  map_sens_fast[grep("^model_sens", names(map_sens_fast))],
  map_sens_complex[grep("^model_sens", names(map_sens_complex))]
)

# Gather Site Sensitivity Models
posteriors_site_sens <- c(
  map_atach_sens[grep("^model_atach", names(map_atach_sens))]
)

# Gather Interaction Models
posteriors_interaction <- c(
  map_interactions[grep("^model_interaction", names(map_interactions))]
)


# 5. PIPELINE -------------------------------------------------------------
tar_plan(
  ## Data & Setup ----
  tar_file_read(imported_data, "data/raw_data/all.rds", read_rds(!!.x)),
  left_fill = "#ce4951",
  right_fill = "#476170",

  selected_data = select_variables(imported_data),
  ich_all = filter_variables(selected_data),
  ich_aggressive = ich_all |>
    filter(study == "ERICH" | study == "ATACH-2") |>
    droplevels(),

  # Imputation Targets
  ich_imputed = f_imputed(ich_aggressive),
  imputed_visualizations = f_plot_imputations_detailed(ich_imputed),

  # DAGs
  dag_neurosurgery = f_neurosurgery_dag(ich_aggressive),
  dag_outcomes = outcomes_dag_function(ich_aggressive),
  settings = model_setup(),

  # Include the Model Maps
  map_main_fast,
  map_main_complex,
  map_sens_fast,
  map_sens_complex,
  map_atach_sens, # Track C
  map_interactions, # Track D

  # Combine Results - Main Analysis
  tar_combine(
    all_main_models,
    posteriors_main,
    command = c(!!!.x)
  ),

  # Combine Results - Sensitivity Analysis
  tar_combine(
    all_sens_models,
    posteriors_sens,
    command = c(!!!.x)
  ),

  # Combine Results - Site Sensitivity
  tar_combine(
    all_site_sens_models,
    posteriors_site_sens,
    command = c(!!!.x)
  ),

  # Combine Results - Interaction Checks
  tar_combine(
    all_interaction_models,
    posteriors_interaction,
    command = c(!!!.x)
  ),

  # Comparison Table: ATACH-2 Sensitivity
  tar_target(
    table_site_comparison,
    tibble(
      model_list = all_site_sens_models,
      model_name = names(all_site_sens_models)
    ) |>
      mutate(
        estimates = purrr::map(
          model_list,
          ~ {
            fit <- .x[[1]]
            broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE) |>
              filter(grepl("laterality", term))
          }
        )
      ) |>
      tidyr::unnest(estimates) |>
      select(model_name, term, estimate, conf.low, conf.high)
  ),

  # Comparison Table: Interaction Results
  tar_target(
    table_interaction_results,
    tibble(
      model_list = all_interaction_models,
      model_name = names(all_interaction_models)
    ) |>
      mutate(
        estimates = purrr::map(
          model_list,
          ~ {
            fit <- .x[[1]]
            # We want to see the interaction term specifically
            broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE) |>
              filter(grepl(":", term)) # Filter for interaction terms
          }
        )
      ) |>
      tidyr::unnest(estimates) |>
      select(model_name, term, estimate, conf.low, conf.high)
  ),

  # Generate Table 2 (Using Main Models)
  tar_map(
    values = table_scenarios,
    tar_target(
      table_2,
      table_2_function(
        x = ich_aggressive,
        models = subset_models_for_table2(all_main_models, scenario)
      )
    )
  ),

  # --------------------
  # Missing Data Analysis
  # --------------------
  tar_target(
    name = missing_data_object,
    command = f_missing_data_filter(ich_aggressive)
  ),

  # Stratified Missingness
  tar_map(
    values = tibble(variable_name = c("ich_laterality", "study")),
    names = "variable_name",

    tar_target(
      name = missing_data_by,
      command = f_percent_missing_visual_stratified(
        missing_data_object,
        variable_name
      )
    )
  ),

  # Shadow Plots
  tar_map(
    values = tidyr::crossing(
      plotting_variable = c(
        "age",
        "gcs_baseline",
        "ich_volume_baseline",
        "time_symptoms_to_ed"
      ),
      missing_variable = c(
        "mrs_90",
        "euro_vas_90",
        "euro_mobility_90",
        "euro_usual_90",
        "euro_pain_90",
        "euro_anxiety_90"
      )
    ),
    names = c("plotting_variable", "missing_variable"),

    tar_target(
      name = missingness_check,
      command = f_shadow_plots(
        ich_aggressive,
        plotting_variable,
        missing_variable
      )
    )
  )
)
