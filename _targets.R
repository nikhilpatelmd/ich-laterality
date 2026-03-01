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
# We rely on callr to manage workers dynamically.
# Run tar_make_future(workers = 20) for full parallel usage.
plan(callr)

tar_option_set(
  garbage_collection = TRUE,
  memory = "transient"
)

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
source("R/sidecar_models.R")
source("R/vas.R")
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
source("R/imputed_data.R")
source("R/sensitivity.R")

# 2. DEFINE GRIDS ---------------------------------------------------------

# Aggressive Care Outcomes
aggressive_grid <- tibble::tribble(
  ~outcome_col          , ~family                          , ~int_mean , ~int_sd , ~complexity ,
  "neurosurgery_evac"   , quote(bernoulli(link = "logit")) ,        -7 , 0.35    , "complex"   ,
  "evd"                 , quote(bernoulli(link = "logit")) ,         0 , 0.5     , "complex"   ,
  # "days_mechanical_ventilation" , quote(negbinomial(link = "log")) ,         0 , 0.5     , "complex"   ,
  "dnr_binary"          , quote(bernoulli(link = "logit")) ,        -5 , 0.5     , "complex"   ,
  "comfort_care_binary" , quote(bernoulli(link = "logit")) ,        -5 , 0.5     , "complex"   ,
  "early_wlst"          , quote(bernoulli(link = "logit")) ,         0 , 0.5     , "complex"   ,
  "tracheostomy"        , quote(bernoulli(link = "logit")) ,       -15 , 0.5     , "complex"
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
  time = c("90")
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

# Combine the base grids first
combined_base <- bind_rows(aggressive_grid, functional_grid)

# THEN cross with scenarios
complete_grid <- tidyr::crossing(
  combined_base,
  prior_scenario = c("neutral", "left", "right", "flat"),
  adjustment_set = c("minimal", "adjusted")
)

grid_fast <- complete_grid |> filter(complexity == "fast")
grid_complex <- complete_grid |> filter(complexity == "complex")

# --- 2a. ADD Ventilation Grid ---
grid_ventilation <- tibble::tribble(
  ~outcome_col                  , ~family                                        , ~int_mean , ~int_sd , ~complexity ,
  "days_mechanical_ventilation" , quote(zero_inflated_negbinomial(link = "log")) ,         0 , 0.5     , "complex"
) |>
  tidyr::crossing(
    prior_scenario = c("neutral", "left", "right", "flat"),
    adjustment_set = c("minimal", "adjusted")
  )

# --- 2b. ADD VAS GRID ---
grid_vas <- tibble::tibble(
  outcome_col = "euro_vas_90",
  family = list(quote(zero_one_inflated_beta()))
) |>
  tidyr::crossing(
    prior_scenario = c("neutral", "left", "right", "flat"),
    adjustment_set = c("minimal", "adjusted")
  )

# ATACH Sensitivity Grid
grid_atach_sens <- aggressive_grid |>
  filter(outcome_col == "neurosurgery_evac") |>
  tidyr::crossing(prior_scenario = "neutral", adjustment_set = "adjusted")

# Interaction Grid
grid_interactions <- tibble(
  outcome_col = "neurosurgery_evac",
  interaction_var = c("ich_location", "study"),
  family = rep(list(quote(bernoulli(link = "logit"))), 2),
  prior_scenario = "neutral",
  adjustment_set = "adjusted"
)

table_scenarios <- tibble(scenario = c("neutral", "left", "right", "flat"))

# 3. DEFINE TARGET LISTS (STEP-BY-STEP) -----------------------------------

# --- A. Data & Setup Targets ---
t_data <- list(
  tar_file_read(imported_data, "data/raw_data/all.rds", read_rds(!!.x)),
  tar_target(left_fill, "#ce4951"),
  tar_target(right_fill, "#476170"),

  tar_target(selected_data, select_variables(imported_data)),
  tar_target(ich_all, filter_variables(selected_data)),

  tar_target(
    ich_aggressive,
    ich_all |> filter(study == "ERICH" | study == "ATACH-2") |> droplevels()
  ),

  tar_target(
    ich_atach,
    ich_aggressive |> filter(study == "ATACH-2") |> droplevels()
  ),

  # --- MODIFIED TARGET: File-Based Imputation ---
  tar_target(
    ich_imputed_file,
    command = {
      # CRITICAL: Force n_imputes = 20 here to override the default 100
      imp_obj <- f_imputed(ich_aggressive, n_imputes = 20)

      # Save to disk
      path <- "data/proc/ich_imputed.rds"
      dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
      saveRDS(imp_obj, path)

      # Return the path
      return(path)
    },
    format = "file" # Tracks the file hash, not the object in RAM
  ),

  # Note: You might need to update this visualization function to read the file too
  # or just create a separate non-file target for visualization if needed.
  tar_target(
    imputed_visualizations,
    f_plot_imputations_detailed(readRDS(ich_imputed_file))
  ),

  tar_target(dag_neurosurgery, f_neurosurgery_dag(ich_aggressive)),
  tar_target(dag_outcomes, outcomes_dag_function(ich_aggressive)),
  tar_target(settings, model_setup())
)

# --- B. Map Definitions ---

# Track A: Main Analysis
map_main_fast <- tar_map(
  values = grid_fast,
  names = c("outcome_col", "prior_scenario", "adjustment_set"),
  unlist = FALSE,
  tar_target(
    model_main,
    list(fit_laterality_model(
      data = ich_imputed_file,
      use_imputation = TRUE,
      outcome_col = outcome_col,
      family = family,
      prior_scenario = prior_scenario,
      adjustment_set = adjustment_set,
      int_mean = int_mean,
      int_sd = int_sd,
      sample_prior = "no",
      settings = model_setup("fast"),
      random_effect_str = "(1 | study)"
    )),
    deployment = "worker"
  )
)

map_main_complex <- tar_map(
  values = grid_complex,
  names = c("outcome_col", "prior_scenario", "adjustment_set"),
  unlist = FALSE,
  tar_target(
    model_main,
    list(fit_laterality_model(
      data = ich_imputed_file,
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
    )),
    deployment = "main"
  )
)

# --- 3. ADD VENTILATION MAP ---
map_main_ventilation <- tar_map(
  values = grid_ventilation,
  names = c("outcome_col", "prior_scenario", "adjustment_set"),
  unlist = FALSE,
  tar_target(
    model_main,
    list(fit_ventilation_zinb(
      data = ich_aggressive, # Unimputed dataset
      use_imputation = FALSE, # Keep imputation off
      outcome_col = outcome_col,
      family = family, # Now passing the ZINB family
      prior_scenario = prior_scenario,
      adjustment_set = adjustment_set,
      int_mean = int_mean,
      int_sd = int_sd,
      sample_prior = "no",
      settings = model_setup("complex"),
      random_effect_str = "(1 | study)"
    )),
    deployment = "main"
  )
)

# --- VAS Main Map (Imputed Data) ---
map_main_vas <- tar_map(
  values = grid_vas,
  names = c("outcome_col", "prior_scenario", "adjustment_set"),
  unlist = FALSE,
  tar_target(
    model_main,
    list(fit_vas_zoib(
      data = ich_imputed_file,
      use_imputation = TRUE,
      prior_scenario = prior_scenario,
      adjustment_set = adjustment_set,
      settings = model_setup("complex"),
      random_effect_str = "(1 | study)"
    )),
    deployment = "main"
  )
)

# Track B: Sensitivity Analysis
map_sens_fast <- tar_map(
  values = grid_fast,
  names = c("outcome_col", "prior_scenario", "adjustment_set"),
  unlist = FALSE,
  tar_target(
    model_sens,
    list(fit_laterality_model(
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
    )),
    deployment = "worker"
  )
)

map_sens_complex <- tar_map(
  values = grid_complex,
  names = c("outcome_col", "prior_scenario", "adjustment_set"),
  unlist = FALSE,
  tar_target(
    model_sens,
    list(fit_laterality_model(
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
    )),
    deployment = "main"
  )
)

# --- VAS Sensitivity Map (Complete Case) ---
map_sens_vas <- tar_map(
  values = grid_vas,
  names = c("outcome_col", "prior_scenario", "adjustment_set"),
  unlist = FALSE,
  tar_target(
    model_sens,
    list(fit_vas_zoib(
      data = ich_aggressive,
      use_imputation = FALSE,
      prior_scenario = prior_scenario,
      adjustment_set = adjustment_set,
      settings = model_setup("complex"),
      random_effect_str = "(1 | study)"
    )),
    deployment = "main"
  )
)

# Track C: Site Sensitivity
map_atach_sens <- tar_map(
  values = grid_atach_sens,
  names = "outcome_col",
  unlist = FALSE,
  tar_target(
    model_atach_base,
    list(fit_laterality_model(
      data = ich_atach,
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
    )),
    deployment = "worker"
  ),
  tar_target(
    model_atach_site,
    list(fit_laterality_model(
      data = ich_atach,
      outcome_col = outcome_col,
      family = family,
      prior_scenario = prior_scenario,
      adjustment_set = adjustment_set,
      int_mean = int_mean,
      int_sd = int_sd,
      sample_prior = "no",
      settings = model_setup("fast"),
      use_imputation = FALSE,
      random_effect_str = "(1 | site_id)"
    )),
    deployment = "worker"
  )
)

# Track D: Interactions
map_interactions <- tar_map(
  values = grid_interactions,
  names = "interaction_var",
  unlist = FALSE,
  tar_target(
    model_interaction,
    list(fit_laterality_model(
      data = ich_aggressive,
      use_imputation = FALSE,
      outcome_col = outcome_col,
      family = family,
      prior_scenario = prior_scenario,
      adjustment_set = adjustment_set,
      interaction_var = interaction_var,
      settings = model_setup("fast")
    )),
    deployment = "worker"
  )
)

# Track E: Prior Predictive Checks
# We map over 'complete_grid' here (instead of just grid_fast) to ensure
# we get priors for every scenario, even if we are running the main model as complex.
map_priors <- tar_map(
  values = complete_grid,
  names = c("outcome_col", "prior_scenario", "adjustment_set"),
  unlist = FALSE,
  tar_target(
    model_prior,
    list(fit_laterality_model(
      data = ich_aggressive,
      use_imputation = FALSE,
      outcome_col = outcome_col,
      family = family,
      prior_scenario = prior_scenario,
      adjustment_set = adjustment_set,
      int_mean = int_mean,
      int_sd = int_sd,

      # CRITICAL: Turn off likelihood to sample priors only
      sample_prior = "only",

      # Use fast settings (no data likelihood = very fast)
      settings = model_setup("fast"),
      random_effect_str = "(1 | study)"
    )),
    deployment = "worker"
  )
)

# --- Ventilation Prior Predictive Checks ---
map_priors_ventilation <- tar_map(
  values = grid_ventilation,
  names = c("outcome_col", "prior_scenario", "adjustment_set"),
  unlist = FALSE,
  tar_target(
    model_prior,
    list(fit_ventilation_zinb(
      data = ich_aggressive,
      use_imputation = FALSE,
      outcome_col = outcome_col,
      family = family,
      prior_scenario = prior_scenario,
      adjustment_set = adjustment_set,
      int_mean = int_mean,
      int_sd = int_sd,
      settings = model_setup("fast"),
      random_effect_str = "(1 | study)",
      sample_prior = "only"
    )),
    deployment = "worker"
  )
)

# --- VAS Prior Predictive Checks ---
map_priors_vas <- tar_map(
  values = grid_vas,
  names = c("outcome_col", "prior_scenario", "adjustment_set"),
  unlist = FALSE,
  tar_target(
    model_prior,
    list(fit_vas_zoib_prior_only(
      data = ich_aggressive, # Use complete-case data for priors to save computation
      use_imputation = FALSE,
      prior_scenario = prior_scenario,
      adjustment_set = adjustment_set,
      settings = model_setup("fast"), # Fast settings since we ignore the data
      random_effect_str = "(1 | study)",
      sample_prior = "only" # CRITICAL: Sample priors only
    )),
    deployment = "worker"
  )
)


# --- C. Combination & Results Targets ---
t_combine <- list(
  tar_combine(
    all_main_models,
    map_main_fast,
    map_main_complex,
    map_main_ventilation,
    map_main_vas,
    command = c(!!!.x)
  ),

  tar_combine(
    all_sens_models,
    map_sens_fast,
    map_sens_complex,
    map_sens_vas,
    command = c(!!!.x)
  ),

  tar_combine(
    all_site_sens_models,
    map_atach_sens,
    command = c(!!!.x)
  ),

  tar_combine(
    all_interaction_models,
    map_interactions,
    command = c(!!!.x)
  ),

  # Combine Prior Models
  tar_combine(
    all_prior_models,
    map_priors,
    map_priors_ventilation,
    map_priors_vas,
    command = c(!!!.x)
  ),

  # Site Comparison Table
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
            fit <- .x
            broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE) |>
              filter(grepl("laterality", term))
          }
        )
      ) |>
      tidyr::unnest(estimates) |>
      select(model_name, term, estimate, conf.low, conf.high)
  ),

  # Interaction Results Table
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
            fit <- .x
            broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE) |>
              filter(grepl(":", term))
          }
        )
      ) |>
      tidyr::unnest(estimates) |>
      select(model_name, term, estimate, conf.low, conf.high)
  )
)

# --- D. Table 2 Map ---
map_table2 <- tar_map(
  values = table_scenarios,
  tar_target(
    table_2,
    table_2_function(
      x = ich_aggressive,
      models = subset_models_for_table2(all_main_models, scenario)
    )
  )
)

# --- D2. Table 2 Priors Map ---
map_table2_priors <- tar_map(
  values = table_scenarios,
  tar_target(
    table_2_priors,
    table_2_priors_function(
      models = subset_prior_models_for_table2(all_prior_models, scenario)
    )
  )
)

# --- D3. Table 4 Map ---
map_table4 <- tar_map(
  values = table_scenarios,
  tar_target(
    table_4,
    table_4_function(
      models = subset_models_for_table4(all_main_models, scenario)
    )
  )
)

# --- D4. Table 4 Priors Map ---
map_table4_priors <- tar_map(
  values = table_scenarios,
  tar_target(
    table_4_priors,
    table_4_priors_function(
      models = subset_prior_models_for_table4(all_prior_models, scenario)
    )
  )
)

# --- E. Missing Data Targets ---
t_missing <- list(
  tar_target(
    name = missing_data_object,
    command = f_missing_data_filter(ich_aggressive)
  ),

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

# 4. FINAL PLAN -----------------------------------------------------------
list(
  t_data,
  map_main_fast,
  map_main_complex,
  map_sens_fast,
  map_sens_complex,
  map_main_ventilation,
  map_main_vas,
  map_sens_vas,
  map_atach_sens,
  map_interactions,
  map_priors,
  map_priors_ventilation,
  map_priors_vas,
  t_combine,
  map_table2,
  map_table2_priors,
  map_table4,
  map_table4_priors,
  t_missing
)
