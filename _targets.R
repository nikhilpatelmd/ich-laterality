library(targets)
library(tarchetypes)
library(tibble)
library(dplyr)
library(brms)
library(quarto)
library(here)
library(future)
library(future.callr)

# PARALLEL PLAN --------------------------------------------------------
plan(callr, workers = 3)

# General pipeline settings ----
suppressPackageStartupMessages(library(brms))
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

# DEFINE GRIDS ---------------------------------------------------------

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
# base_grid <- bind_rows(aggressive_grid, functional_grid) # Uncomment when ready

# SPLIT GRID FOR DEPLOYMENT --------------------------------------------

complete_grid <- tidyr::crossing(
  base_grid,
  prior_scenario = c("neutral", "left", "right", "flat"),
  adjustment_set = c("minimal", "adjusted")
)

# Separate the tasks based on complexity
grid_fast <- complete_grid |> filter(complexity == "fast")
grid_complex <- complete_grid |> filter(complexity == "complex")

# Pipeline ----
tar_plan(
  ## Define raw data file ----
  tar_file_read(imported_data, "data/raw_data/all.rds", read_rds(!!.x)),

  ## Figure Themes and Colors ----
  left_fill = "#ce4951",
  right_fill = "#476170",
  theme_ich = theme_ich(),

  ## Select, filter, and clean data ----
  selected_data = select_variables(imported_data),
  ich_all = filter_variables(selected_data),
  ich_aggressive = ich_all |>
    filter(study == "ERICH" | study == "ATACH-2") |>
    droplevels(),
  ich_imputed = f_imputed(ich_aggressive),

  ## DAGs ----
  dag_neurosurgery = f_neurosurgery_dag(x),
  dag_outcomes = outcomes_dag_function(x),

  settings = model_setup(),

  # MAP 1: FAST MODELS (Run on Workers)
  # -------------------------------------------------------------------------
  tar_map(
    values = grid_fast,
    names = c("outcome_col", "prior_scenario", "adjustment_set"),

    # 1. Posterior (Fast -> Worker)
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
      deployment = "worker" # <--- Hardcoded, Safe
    ),

    # 2. Prior Check (Always Fast -> Worker)
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
  ),

  # MAP 2: COMPLEX MODELS (Run on Main Process)
  # -------------------------------------------------------------------------
  tar_map(
    values = grid_complex,
    names = c("outcome_col", "prior_scenario", "adjustment_set"),

    # 1. Posterior (Complex -> Main)
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
      deployment = "main" # <--- Hardcoded, Safe
    ),

    # 2. Prior Check (Always Fast -> Worker)
    # Even for complex outcomes, the prior check is fast, so we can send it to a worker.
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
)

#   ## Aggressive Care ----

#   ### Priors ----
#   settings = model_setup(),
#   m_prior_neutral_neurosurgery = f_prior_neutral_neurosurgery(ich_aggressive),
#   m_prior_left_neurosurgery = f_prior_left_neurosurgery(ich_aggressive),
#   m_prior_right_neurosurgery = f_prior_right_neurosurgery(ich_aggressive),
#   m_prior_flat_neurosurgery = f_prior_flat_neurosurgery(ich_aggressive),
#   m_prior_neutral_evd = f_prior_neutral_evd(ich_aggressive),
#   m_prior_left_evd = f_prior_left_evd(ich_aggressive),
#   m_prior_right_evd = f_prior_right_evd(ich_aggressive),
#   m_prior_flat_evd = f_prior_flat_evd(ich_aggressive),
#   m_prior_neutral_tracheostomy = f_prior_neutral_tracheostomy(ich_aggressive),
#   m_prior_left_tracheostomy = f_prior_left_tracheostomy(ich_aggressive),
#   m_prior_right_tracheostomy = f_prior_right_tracheostomy(ich_aggressive),
#   m_prior_flat_tracheostomy = f_prior_flat_tracheostomy(ich_aggressive),
#   m_prior_neutral_days_mechanical_ventilation = f_prior_neutral_days_mechanical_ventilation(
#     ich_aggressive
#   ),
#   m_prior_left_days_mechanical_ventilation = f_prior_left_days_mechanical_ventilation(
#     ich_aggressive
#   ),
#   m_prior_right_days_mechanical_ventilation = f_prior_right_days_mechanical_ventilation(
#     ich_aggressive
#   ),
#   m_prior_flat_days_mechanical_ventilation = f_prior_flat_days_mechanical_ventilation(
#     ich_aggressive
#   ),
#   m_prior_neutral_comfort = f_prior_neutral_comfort(ich_aggressive),
#   m_prior_left_comfort = f_prior_left_comfort(ich_aggressive),
#   m_prior_right_comfort = f_prior_right_comfort(ich_aggressive),
#   m_prior_flat_comfort = f_prior_flat_comfort(ich_aggressive),
#   m_prior_neutral_early_wlst = f_prior_neutral_early_wlst(ich_aggressive),
#   m_prior_left_early_wlst = f_prior_left_early_wlst(ich_aggressive),
#   m_prior_right_early_wlst = f_prior_right_early_wlst(ich_aggressive),
#   m_prior_flat_early_wlst = f_prior_flat_early_wlst(ich_aggressive),
#   m_prior_neutral_dnr_binary = f_prior_neutral_dnr_binary(ich_aggressive),
#   m_prior_left_dnr_binary = f_prior_left_dnr_binary(ich_aggressive),
#   m_prior_right_dnr_binary = f_prior_right_dnr_binary(ich_aggressive),
#   m_prior_flat_dnr_binary = f_prior_flat_dnr_binary(ich_aggressive),

#   ### Prior Predictive Checks ----
#   prior_check_neurosurgery = f_prior_predictive_check(
#     m_prior_neutral_neurosurgery,
#     m_prior_left_neurosurgery,
#     m_prior_right_neurosurgery,
#     m_prior_flat_neurosurgery
#   ),
#   prior_visual_neurosurgery_neutral = prediction_visual(
#     m_prior_neutral_neurosurgery
#   ),
#   prior_visual_neurosurgery_left = prediction_visual(m_prior_left_neurosurgery),
#   prior_visual_neurosurgery_right = prediction_visual(
#     m_prior_right_neurosurgery
#   ),
#   prior_visual_neurosurgery_flat = prediction_visual(m_prior_flat_neurosurgery),
#   prior_check_evd = f_prior_predictive_check(
#     m_prior_neutral_evd,
#     m_prior_left_evd,
#     m_prior_right_evd,
#     m_prior_flat_evd
#   ),
#   prior_visual_evd_neutral = prediction_visual(m_prior_neutral_evd),
#   prior_visual_evd_left = prediction_visual(m_prior_left_evd),
#   prior_visual_evd_right = prediction_visual(m_prior_right_evd),
#   prior_visual_evd_flat = prediction_visual(m_prior_flat_evd),
#   prior_check_tracheostomy = f_prior_predictive_check(
#     m_prior_neutral_tracheostomy,
#     m_prior_left_tracheostomy,
#     m_prior_right_tracheostomy,
#     m_prior_flat_tracheostomy
#   ),
#   prior_visual_tracheostomy_neutral = prediction_visual(
#     m_prior_neutral_tracheostomy
#   ),
#   prior_visual_tracheostomy_left = prediction_visual(m_prior_left_tracheostomy),
#   prior_visual_tracheostomy_right = prediction_visual(
#     m_prior_right_tracheostomy
#   ),
#   prior_visual_tracheostomy_flat = prediction_visual(m_prior_flat_tracheostomy),
#   prior_check_days_mechanical_ventilation = f_prior_predictive_check_vent(
#     m_prior_neutral_days_mechanical_ventilation,
#     m_prior_left_days_mechanical_ventilation,
#     m_prior_right_days_mechanical_ventilation,
#     m_prior_flat_days_mechanical_ventilation
#   ),
#   prior_visual_days_mechanical_ventilation_neutral = prediction_visual(
#     m_prior_neutral_days_mechanical_ventilation
#   ),
#   prior_visual_days_mechanical_ventilation_left = prediction_visual(
#     m_prior_left_days_mechanical_ventilation
#   ),
#   prior_visual_days_mechanical_ventilation_right = prediction_visual(
#     m_prior_right_days_mechanical_ventilation
#   ),
#   prior_visual_days_mechanical_ventilation_flat = prediction_visual(
#     m_prior_flat_days_mechanical_ventilation
#   ),
#   prior_check_comfort = f_prior_predictive_check(
#     m_prior_neutral_comfort,
#     m_prior_left_comfort,
#     m_prior_right_comfort,
#     m_prior_flat_comfort
#   ),
#   prior_visual_comfort_neutral = prediction_visual(m_prior_neutral_comfort),
#   prior_visual_comfort_left = prediction_visual(m_prior_left_comfort),
#   prior_visual_comfort_right = prediction_visual(m_prior_right_comfort),
#   prior_visual_comfort_flat = prediction_visual(m_prior_flat_comfort),
#   prior_check_early_wlst = f_prior_predictive_check(
#     m_prior_neutral_early_wlst,
#     m_prior_left_early_wlst,
#     m_prior_right_early_wlst,
#     m_prior_flat_early_wlst
#   ),
#   prior_visual_early_wlst_neutral = prediction_visual(
#     m_prior_neutral_early_wlst
#   ),
#   prior_visual_early_wlst_left = prediction_visual(m_prior_left_early_wlst),
#   prior_visual_early_wlst_right = prediction_visual(m_prior_right_early_wlst),
#   prior_visual_early_wlst_flat = prediction_visual(m_prior_flat_early_wlst),
#   prior_check_dnr_binary = f_prior_predictive_check(
#     m_prior_neutral_dnr_binary,
#     m_prior_left_dnr_binary,
#     m_prior_right_dnr_binary,
#     m_prior_flat_dnr_binary
#   ),
#   prior_visual_dnr_binary_neutral = prediction_visual(
#     m_prior_neutral_dnr_binary
#   ),
#   prior_visual_dnr_binary_left = prediction_visual(m_prior_left_dnr_binary),
#   prior_visual_dnr_binary_right = prediction_visual(m_prior_right_dnr_binary),
#   prior_visual_dnr_binary_flat = prediction_visual(m_prior_flat_dnr_binary),

#   ### Posterior Simulation ----
#   m_posterior_neutral_neurosurgery = f_posterior_neutral_neurosurgery(
#     ich_aggressive
#   ),
#   m_posterior_left_neurosurgery = f_posterior_left_neurosurgery(ich_aggressive),
#   m_posterior_right_neurosurgery = f_posterior_right_neurosurgery(
#     ich_aggressive
#   ),
#   m_posterior_flat_neurosurgery = f_posterior_flat_neurosurgery(ich_aggressive),
#   m_posterior_neutral_evd = f_posterior_neutral_evd(ich_aggressive),
#   m_posterior_left_evd = f_posterior_left_evd(ich_aggressive),
#   m_posterior_right_evd = f_posterior_right_evd(ich_aggressive),
#   m_posterior_flat_evd = f_posterior_flat_evd(ich_aggressive),
#   m_posterior_neutral_tracheostomy = f_posterior_neutral_tracheostomy(
#     ich_aggressive
#   ),
#   m_posterior_left_tracheostomy = f_posterior_left_tracheostomy(ich_aggressive),
#   m_posterior_right_tracheostomy = f_posterior_right_tracheostomy(
#     ich_aggressive
#   ),
#   m_posterior_flat_tracheostomy = f_posterior_flat_tracheostomy(ich_aggressive),
#   m_posterior_neutral_days_mechanical_ventilation = f_posterior_neutral_days_mechanical_ventilation(
#     ich_aggressive
#   ),
#   m_posterior_left_days_mechanical_ventilation = f_posterior_left_days_mechanical_ventilation(
#     ich_aggressive
#   ),
#   m_posterior_right_days_mechanical_ventilation = f_posterior_right_days_mechanical_ventilation(
#     ich_aggressive
#   ),
#   m_posterior_flat_days_mechanical_ventilation = f_posterior_flat_days_mechanical_ventilation(
#     ich_aggressive
#   ),
#   m_posterior_neutral_comfort = f_posterior_neutral_comfort(ich_aggressive),
#   m_posterior_left_comfort = f_posterior_left_comfort(ich_aggressive),
#   m_posterior_right_comfort = f_posterior_right_comfort(ich_aggressive),
#   m_posterior_flat_comfort = f_posterior_flat_comfort(ich_aggressive),
#   m_posterior_neutral_early_wlst = f_posterior_neutral_early_wlst(
#     ich_aggressive
#   ),
#   m_posterior_left_early_wlst = f_posterior_left_early_wlst(ich_aggressive),
#   m_posterior_right_early_wlst = f_posterior_right_early_wlst(ich_aggressive),
#   m_posterior_flat_early_wlst = f_posterior_flat_early_wlst(ich_aggressive),
#   m_posterior_neutral_dnr_binary = f_posterior_neutral_dnr_binary(
#     ich_aggressive
#   ),
#   m_posterior_left_dnr_binary = f_posterior_left_dnr_binary(ich_aggressive),
#   m_posterior_right_dnr_binary = f_posterior_right_dnr_binary(ich_aggressive),
#   m_posterior_flat_dnr_binary = f_posterior_flat_dnr_binary(ich_aggressive),

#   #### Posterior Diagnostics
#   diag_posterior_neutral_neurosurgery = posterior_diagnostics(
#     m_posterior_neutral_neurosurgery
#   ),
#   diag_posterior_neutral_evd = posterior_diagnostics(m_posterior_neutral_evd),
#   diag_posterior_neutral_tracheostomy = posterior_diagnostics(
#     m_posterior_neutral_tracheostomy
#   ),
#   diag_posterior_neutral_days_mechanical_ventilation = posterior_diagnostics(
#     m_posterior_neutral_days_mechanical_ventilation
#   ),
#   diag_posterior_neutral_comfort = posterior_diagnostics(
#     m_posterior_neutral_comfort
#   ),
#   diag_posterior_neutral_early_wlst = posterior_diagnostics(
#     m_posterior_neutral_early_wlst
#   ),
#   diag_posterior_neutral_dnr_binary = posterior_diagnostics(
#     m_posterior_neutral_dnr_binary
#   ),

#   #### Posterior Predictive Checks
#   post_pred_check_neurosurgery = f_post_predictive_check(
#     m_posterior_neutral_neurosurgery
#   ),
#   post_pred_check_evd = f_post_predictive_check(m_posterior_neutral_evd),
#   post_pred_check_tracheostomy = f_post_predictive_check(
#     m_posterior_neutral_tracheostomy
#   ),
#   post_pred_check_days_mechanical_ventilation = f_post_predictive_check(
#     m_posterior_neutral_days_mechanical_ventilation
#   ),
#   post_pred_check_comfort = f_post_predictive_check(
#     m_posterior_neutral_comfort
#   ),
#   post_pred_check_early_wlst = f_post_predictive_check(
#     m_posterior_neutral_early_wlst
#   ),
#   post_pred_check_dnr_binary = f_post_predictive_check(
#     m_posterior_neutral_dnr_binary
#   ),

#   ## Outcomes ----

#   ### Priors ----
#   m_prior_mrs_90_minimal = f_prior_mrs_90_minimal(ich_aggressive),
#   m_prior_neutral_mrs_90_canonical = f_prior_neutral_mrs_90_canonical(
#     ich_aggressive
#   ),
#   m_prior_left_mrs_90_canonical = f_prior_left_mrs_90_canonical(ich_aggressive),
#   m_prior_right_mrs_90_canonical = f_prior_right_mrs_90_canonical(
#     ich_aggressive
#   ),
#   m_prior_flat_mrs_90_canonical = f_prior_flat_mrs_90_canonical(ich_aggressive),
#   m_prior_mrs_180_minimal = f_prior_mrs_180_minimal(ich_aggressive),
#   m_prior_neutral_mrs_180_canonical = f_prior_neutral_mrs_180_canonical(
#     ich_aggressive
#   ),
#   m_prior_left_mrs_180_canonical = f_prior_left_mrs_180_canonical(
#     ich_aggressive
#   ),
#   m_prior_right_mrs_180_canonical = f_prior_right_mrs_180_canonical(
#     ich_aggressive
#   ),
#   m_prior_flat_mrs_180_canonical = f_prior_flat_mrs_180_canonical(
#     ich_aggressive
#   ),
#   m_prior_mrs_365_minimal = f_prior_mrs_365_minimal(ich_aggressive),
#   m_prior_neutral_mrs_365_canonical = f_prior_neutral_mrs_365_canonical(
#     ich_aggressive
#   ),
#   m_prior_left_mrs_365_canonical = f_prior_left_mrs_365_canonical(
#     ich_aggressive
#   ),
#   m_prior_right_mrs_365_canonical = f_prior_right_mrs_365_canonical(
#     ich_aggressive
#   ),
#   m_prior_flat_mrs_365_canonical = f_prior_flat_mrs_365_canonical(
#     ich_aggressive
#   ),
#   m_prior_neutral_euro_mobility_90_canonical = f_prior_neutral_euro_mobility_90_canonical(
#     ich_aggressive
#   ),
#   m_prior_left_euro_mobility_90_canonical = f_prior_left_euro_mobility_90_canonical(
#     ich_aggressive
#   ),
#   m_prior_right_euro_mobility_90_canonical = f_prior_right_euro_mobility_90_canonical(
#     ich_aggressive
#   ),
#   m_prior_flat_euro_mobility_90_canonical = f_prior_flat_euro_mobility_90_canonical(
#     ich_aggressive
#   ),
#   m_prior_neutral_euro_mobility_180_canonical = f_prior_neutral_euro_mobility_180_canonical(
#     ich_aggressive
#   ),
#   m_prior_left_euro_mobility_180_canonical = f_prior_left_euro_mobility_180_canonical(
#     ich_aggressive
#   ),
#   m_prior_right_euro_mobility_180_canonical = f_prior_right_euro_mobility_180_canonical(
#     ich_aggressive
#   ),
#   m_prior_flat_euro_mobility_180_canonical = f_prior_flat_euro_mobility_180_canonical(
#     ich_aggressive
#   ),
#   m_prior_neutral_euro_mobility_365_canonical = f_prior_neutral_euro_mobility_365_canonical(
#     ich_aggressive
#   ),
#   m_prior_left_euro_mobility_365_canonical = f_prior_left_euro_mobility_365_canonical(
#     ich_aggressive
#   ),
#   m_prior_right_euro_mobility_365_canonical = f_prior_right_euro_mobility_365_canonical(
#     ich_aggressive
#   ),
#   m_prior_flat_euro_mobility_365_canonical = f_prior_flat_euro_mobility_365_canonical(
#     ich_aggressive
#   ),
#   m_prior_neutral_euro_selfcare_90_canonical = f_prior_neutral_euro_selfcare_90_canonical(
#     ich_aggressive
#   ),
#   m_prior_left_euro_selfcare_90_canonical = f_prior_left_euro_selfcare_90_canonical(
#     ich_aggressive
#   ),
#   m_prior_right_euro_selfcare_90_canonical = f_prior_right_euro_selfcare_90_canonical(
#     ich_aggressive
#   ),
#   m_prior_flat_euro_selfcare_90_canonical = f_prior_flat_euro_selfcare_90_canonical(
#     ich_aggressive
#   ),
#   m_prior_neutral_euro_selfcare_180_canonical = f_prior_neutral_euro_selfcare_180_canonical(
#     ich_aggressive
#   ),
#   m_prior_left_euro_selfcare_180_canonical = f_prior_left_euro_selfcare_180_canonical(
#     ich_aggressive
#   ),
#   m_prior_right_euro_selfcare_180_canonical = f_prior_right_euro_selfcare_180_canonical(
#     ich_aggressive
#   ),
#   m_prior_flat_euro_selfcare_180_canonical = f_prior_flat_euro_selfcare_180_canonical(
#     ich_aggressive
#   ),
#   m_prior_neutral_euro_selfcare_365_canonical = f_prior_neutral_euro_selfcare_365_canonical(
#     ich_aggressive
#   ),
#   m_prior_left_euro_selfcare_365_canonical = f_prior_left_euro_selfcare_365_canonical(
#     ich_aggressive
#   ),
#   m_prior_right_euro_selfcare_365_canonical = f_prior_right_euro_selfcare_365_canonical(
#     ich_aggressive
#   ),
#   m_prior_flat_euro_selfcare_365_canonical = f_prior_flat_euro_selfcare_365_canonical(
#     ich_aggressive
#   ),
#   m_prior_neutral_euro_usual_90_canonical = f_prior_neutral_euro_usual_90_canonical(
#     ich_aggressive
#   ),
#   m_prior_left_euro_usual_90_canonical = f_prior_left_euro_usual_90_canonical(
#     ich_aggressive
#   ),
#   m_prior_right_euro_usual_90_canonical = f_prior_right_euro_usual_90_canonical(
#     ich_aggressive
#   ),
#   m_prior_flat_euro_usual_90_canonical = f_prior_flat_euro_usual_90_canonical(
#     ich_aggressive
#   ),
#   m_prior_neutral_euro_usual_180_canonical = f_prior_neutral_euro_usual_180_canonical(
#     ich_aggressive
#   ),
#   m_prior_left_euro_usual_180_canonical = f_prior_left_euro_usual_180_canonical(
#     ich_aggressive
#   ),
#   m_prior_right_euro_usual_180_canonical = f_prior_right_euro_usual_180_canonical(
#     ich_aggressive
#   ),
#   m_prior_flat_euro_usual_180_canonical = f_prior_flat_euro_usual_180_canonical(
#     ich_aggressive
#   ),
#   m_prior_neutral_euro_usual_365_canonical = f_prior_neutral_euro_usual_365_canonical(
#     ich_aggressive
#   ),
#   m_prior_left_euro_usual_365_canonical = f_prior_left_euro_usual_365_canonical(
#     ich_aggressive
#   ),
#   m_prior_right_euro_usual_365_canonical = f_prior_right_euro_usual_365_canonical(
#     ich_aggressive
#   ),
#   m_prior_flat_euro_usual_365_canonical = f_prior_flat_euro_usual_365_canonical(
#     ich_aggressive
#   ),
#   m_prior_neutral_euro_pain_90_canonical = f_prior_neutral_euro_pain_90_canonical(
#     ich_aggressive
#   ),
#   m_prior_left_euro_pain_90_canonical = f_prior_left_euro_pain_90_canonical(
#     ich_aggressive
#   ),
#   m_prior_right_euro_pain_90_canonical = f_prior_right_euro_pain_90_canonical(
#     ich_aggressive
#   ),
#   m_prior_flat_euro_pain_90_canonical = f_prior_flat_euro_pain_90_canonical(
#     ich_aggressive
#   ),
#   m_prior_neutral_euro_pain_180_canonical = f_prior_neutral_euro_pain_180_canonical(
#     ich_aggressive
#   ),
#   m_prior_left_euro_pain_180_canonical = f_prior_left_euro_pain_180_canonical(
#     ich_aggressive
#   ),
#   m_prior_right_euro_pain_180_canonical = f_prior_right_euro_pain_180_canonical(
#     ich_aggressive
#   ),
#   m_prior_flat_euro_pain_180_canonical = f_prior_flat_euro_pain_180_canonical(
#     ich_aggressive
#   ),
#   m_prior_neutral_euro_pain_365_canonical = f_prior_neutral_euro_pain_365_canonical(
#     ich_aggressive
#   ),
#   m_prior_left_euro_pain_365_canonical = f_prior_left_euro_pain_365_canonical(
#     ich_aggressive
#   ),
#   m_prior_right_euro_pain_365_canonical = f_prior_right_euro_pain_365_canonical(
#     ich_aggressive
#   ),
#   m_prior_flat_euro_pain_365_canonical = f_prior_flat_euro_pain_365_canonical(
#     ich_aggressive
#   ),
#   m_prior_neutral_euro_anxiety_90_canonical = f_prior_neutral_euro_anxiety_90_canonical(
#     ich_aggressive
#   ),
#   m_prior_left_euro_anxiety_90_canonical = f_prior_left_euro_anxiety_90_canonical(
#     ich_aggressive
#   ),
#   m_prior_right_euro_anxiety_90_canonical = f_prior_right_euro_anxiety_90_canonical(
#     ich_aggressive
#   ),
#   m_prior_flat_euro_anxiety_90_canonical = f_prior_flat_euro_anxiety_90_canonical(
#     ich_aggressive
#   ),
#   m_prior_neutral_euro_anxiety_180_canonical = f_prior_neutral_euro_anxiety_180_canonical(
#     ich_aggressive
#   ),
#   m_prior_left_euro_anxiety_180_canonical = f_prior_left_euro_anxiety_180_canonical(
#     ich_aggressive
#   ),
#   m_prior_right_euro_anxiety_180_canonical = f_prior_right_euro_anxiety_180_canonical(
#     ich_aggressive
#   ),
#   m_prior_flat_euro_anxiety_180_canonical = f_prior_flat_euro_anxiety_180_canonical(
#     ich_aggressive
#   ),
#   m_prior_neutral_euro_anxiety_365_canonical = f_prior_neutral_euro_anxiety_365_canonical(
#     ich_aggressive
#   ),
#   m_prior_left_euro_anxiety_365_canonical = f_prior_left_euro_anxiety_365_canonical(
#     ich_aggressive
#   ),
#   m_prior_right_euro_anxiety_365_canonical = f_prior_right_euro_anxiety_365_canonical(
#     ich_aggressive
#   ),
#   m_prior_flat_euro_anxiety_365_canonical = f_prior_flat_euro_anxiety_365_canonical(
#     ich_aggressive
#   ),

#   ### Prior Predictive Checks ----
#   prior_check_mrs_90 = f_prior_predictive_check_vent(
#     m_prior_neutral_mrs_90_canonical,
#     m_prior_left_mrs_90_canonical,
#     m_prior_right_mrs_90_canonical,
#     m_prior_flat_mrs_90_canonical
#   ),
#   prior_visual_mrs_90_neutral = prediction_visual(
#     m_prior_neutral_mrs_90_canonical
#   ),
#   prior_visual_mrs_90_left = prediction_visual(m_prior_left_mrs_90_canonical),
#   prior_visual_mrs_90_right = prediction_visual(m_prior_right_mrs_90_canonical),
#   prior_visual_mrs_90_flat = prediction_visual(m_prior_flat_mrs_90_canonical),
#   prior_check_mrs_180 = f_prior_predictive_check_vent(
#     m_prior_neutral_mrs_180_canonical,
#     m_prior_left_mrs_180_canonical,
#     m_prior_right_mrs_180_canonical,
#     m_prior_flat_mrs_180_canonical
#   ),
#   prior_visual_mrs_180_neutral = prediction_visual(
#     m_prior_neutral_mrs_180_canonical
#   ),
#   prior_visual_mrs_180_left = prediction_visual(m_prior_left_mrs_180_canonical),
#   prior_visual_mrs_180_right = prediction_visual(
#     m_prior_right_mrs_180_canonical
#   ),
#   prior_visual_mrs_180_flat = prediction_visual(m_prior_flat_mrs_180_canonical),
#   prior_check_mrs_365 = f_prior_predictive_check_vent(
#     m_prior_neutral_mrs_365_canonical,
#     m_prior_left_mrs_365_canonical,
#     m_prior_right_mrs_365_canonical,
#     m_prior_flat_mrs_365_canonical
#   ),
#   prior_visual_mrs_365_neutral = prediction_visual(
#     m_prior_neutral_mrs_365_canonical
#   ),
#   prior_visual_mrs_365_left = prediction_visual(m_prior_left_mrs_365_canonical),
#   prior_visual_mrs_365_right = prediction_visual(
#     m_prior_right_mrs_365_canonical
#   ),
#   prior_visual_mrs_365_flat = prediction_visual(m_prior_flat_mrs_365_canonical),
#   prior_check_euro_mobility_90 = f_prior_predictive_check_vent(
#     m_prior_neutral_euro_mobility_90_canonical,
#     m_prior_left_euro_mobility_90_canonical,
#     m_prior_right_euro_mobility_90_canonical,
#     m_prior_flat_euro_mobility_90_canonical
#   ),
#   prior_visual_euro_mobility_90_neutral = prediction_visual(
#     m_prior_neutral_euro_mobility_90_canonical
#   ),
#   prior_visual_euro_mobility_90_left = prediction_visual(
#     m_prior_left_euro_mobility_90_canonical
#   ),
#   prior_visual_euro_mobility_90_right = prediction_visual(
#     m_prior_right_euro_mobility_90_canonical
#   ),
#   prior_visual_euro_mobility_90_flat = prediction_visual(
#     m_prior_flat_euro_mobility_90_canonical
#   ),
#   prior_check_euro_mobility_180 = f_prior_predictive_check_vent(
#     m_prior_neutral_euro_mobility_180_canonical,
#     m_prior_left_euro_mobility_180_canonical,
#     m_prior_right_euro_mobility_180_canonical,
#     m_prior_flat_euro_mobility_180_canonical
#   ),
#   prior_visual_euro_mobility_180_neutral = prediction_visual(
#     m_prior_neutral_euro_mobility_180_canonical
#   ),
#   prior_visual_euro_mobility_180_left = prediction_visual(
#     m_prior_left_euro_mobility_180_canonical
#   ),
#   prior_visual_euro_mobility_180_right = prediction_visual(
#     m_prior_right_euro_mobility_180_canonical
#   ),
#   prior_visual_euro_mobility_180_flat = prediction_visual(
#     m_prior_flat_euro_mobility_180_canonical
#   ),
#   prior_check_euro_mobility_365 = f_prior_predictive_check_vent(
#     m_prior_neutral_euro_mobility_365_canonical,
#     m_prior_left_euro_mobility_365_canonical,
#     m_prior_right_euro_mobility_365_canonical,
#     m_prior_flat_euro_mobility_365_canonical
#   ),
#   prior_visual_euro_mobility_365_neutral = prediction_visual(
#     m_prior_neutral_euro_mobility_365_canonical
#   ),
#   prior_visual_euro_mobility_365_left = prediction_visual(
#     m_prior_left_euro_mobility_365_canonical
#   ),
#   prior_visual_euro_mobility_365_right = prediction_visual(
#     m_prior_right_euro_mobility_365_canonical
#   ),
#   prior_visual_euro_mobility_365_flat = prediction_visual(
#     m_prior_flat_euro_mobility_365_canonical
#   ),
#   prior_check_euro_usual_90 = f_prior_predictive_check_vent(
#     m_prior_neutral_euro_usual_90_canonical,
#     m_prior_left_euro_usual_90_canonical,
#     m_prior_right_euro_usual_90_canonical,
#     m_prior_flat_euro_usual_90_canonical
#   ),
#   prior_visual_euro_usual_90_neutral = prediction_visual(
#     m_prior_neutral_euro_usual_90_canonical
#   ),
#   prior_visual_euro_usual_90_left = prediction_visual(
#     m_prior_left_euro_usual_90_canonical
#   ),
#   prior_visual_euro_usual_90_right = prediction_visual(
#     m_prior_right_euro_usual_90_canonical
#   ),
#   prior_visual_euro_usual_90_flat = prediction_visual(
#     m_prior_flat_euro_usual_90_canonical
#   ),
#   prior_check_euro_usual_180 = f_prior_predictive_check_vent(
#     m_prior_neutral_euro_usual_180_canonical,
#     m_prior_left_euro_usual_180_canonical,
#     m_prior_right_euro_usual_180_canonical,
#     m_prior_flat_euro_usual_180_canonical
#   ),
#   prior_visual_euro_usual_180_neutral = prediction_visual(
#     m_prior_neutral_euro_usual_180_canonical
#   ),
#   prior_visual_euro_usual_180_left = prediction_visual(
#     m_prior_left_euro_usual_180_canonical
#   ),
#   prior_visual_euro_usual_180_right = prediction_visual(
#     m_prior_right_euro_usual_180_canonical
#   ),
#   prior_visual_euro_usual_180_flat = prediction_visual(
#     m_prior_flat_euro_usual_180_canonical
#   ),
#   prior_check_euro_usual_365 = f_prior_predictive_check_vent(
#     m_prior_neutral_euro_usual_365_canonical,
#     m_prior_left_euro_usual_365_canonical,
#     m_prior_right_euro_usual_365_canonical,
#     m_prior_flat_euro_usual_365_canonical
#   ),
#   prior_visual_euro_usual_365_neutral = prediction_visual(
#     m_prior_neutral_euro_usual_365_canonical
#   ),
#   prior_visual_euro_usual_365_left = prediction_visual(
#     m_prior_left_euro_usual_365_canonical
#   ),
#   prior_visual_euro_usual_365_right = prediction_visual(
#     m_prior_right_euro_usual_365_canonical
#   ),
#   prior_visual_euro_usual_365_flat = prediction_visual(
#     m_prior_flat_euro_usual_365_canonical
#   ),
#   prior_check_euro_selfcare_90 = f_prior_predictive_check_vent(
#     m_prior_neutral_euro_selfcare_90_canonical,
#     m_prior_left_euro_selfcare_90_canonical,
#     m_prior_right_euro_selfcare_90_canonical,
#     m_prior_flat_euro_selfcare_90_canonical
#   ),
#   prior_visual_euro_selfcare_90_neutral = prediction_visual(
#     m_prior_neutral_euro_selfcare_90_canonical
#   ),
#   prior_visual_euro_selfcare_90_left = prediction_visual(
#     m_prior_left_euro_selfcare_90_canonical
#   ),
#   prior_visual_euro_selfcare_90_right = prediction_visual(
#     m_prior_right_euro_selfcare_90_canonical
#   ),
#   prior_visual_euro_selfcare_90_flat = prediction_visual(
#     m_prior_flat_euro_selfcare_90_canonical
#   ),
#   prior_check_euro_selfcare_180 = f_prior_predictive_check_vent(
#     m_prior_neutral_euro_selfcare_180_canonical,
#     m_prior_left_euro_selfcare_180_canonical,
#     m_prior_right_euro_selfcare_180_canonical,
#     m_prior_flat_euro_selfcare_180_canonical
#   ),
#   prior_visual_euro_selfcare_180_neutral = prediction_visual(
#     m_prior_neutral_euro_selfcare_180_canonical
#   ),
#   prior_visual_euro_selfcare_180_left = prediction_visual(
#     m_prior_left_euro_selfcare_180_canonical
#   ),
#   prior_visual_euro_selfcare_180_right = prediction_visual(
#     m_prior_right_euro_selfcare_180_canonical
#   ),
#   prior_visual_euro_selfcare_180_flat = prediction_visual(
#     m_prior_flat_euro_selfcare_180_canonical
#   ),
#   prior_check_euro_selfcare_365 = f_prior_predictive_check_vent(
#     m_prior_neutral_euro_selfcare_365_canonical,
#     m_prior_left_euro_selfcare_365_canonical,
#     m_prior_right_euro_selfcare_365_canonical,
#     m_prior_flat_euro_selfcare_365_canonical
#   ),
#   prior_visual_euro_selfcare_365_neutral = prediction_visual(
#     m_prior_neutral_euro_selfcare_365_canonical
#   ),
#   prior_visual_euro_selfcare_365_left = prediction_visual(
#     m_prior_left_euro_selfcare_365_canonical
#   ),
#   prior_visual_euro_selfcare_365_right = prediction_visual(
#     m_prior_right_euro_selfcare_365_canonical
#   ),
#   prior_visual_euro_selfcare_365_flat = prediction_visual(
#     m_prior_flat_euro_selfcare_365_canonical
#   ),
#   prior_check_euro_pain_90 = f_prior_predictive_check_vent(
#     m_prior_neutral_euro_pain_90_canonical,
#     m_prior_left_euro_pain_90_canonical,
#     m_prior_right_euro_pain_90_canonical,
#     m_prior_flat_euro_pain_90_canonical
#   ),
#   prior_visual_euro_pain_90_neutral = prediction_visual(
#     m_prior_neutral_euro_pain_90_canonical
#   ),
#   prior_visual_euro_pain_90_left = prediction_visual(
#     m_prior_left_euro_pain_90_canonical
#   ),
#   prior_visual_euro_pain_90_right = prediction_visual(
#     m_prior_right_euro_pain_90_canonical
#   ),
#   prior_visual_euro_pain_90_flat = prediction_visual(
#     m_prior_flat_euro_pain_90_canonical
#   ),
#   prior_check_euro_pain_180 = f_prior_predictive_check_vent(
#     m_prior_neutral_euro_pain_180_canonical,
#     m_prior_left_euro_pain_180_canonical,
#     m_prior_right_euro_pain_180_canonical,
#     m_prior_flat_euro_pain_180_canonical
#   ),
#   prior_visual_euro_pain_180_neutral = prediction_visual(
#     m_prior_neutral_euro_pain_180_canonical
#   ),
#   prior_visual_euro_pain_180_left = prediction_visual(
#     m_prior_left_euro_pain_180_canonical
#   ),
#   prior_visual_euro_pain_180_right = prediction_visual(
#     m_prior_right_euro_pain_180_canonical
#   ),
#   prior_visual_euro_pain_180_flat = prediction_visual(
#     m_prior_flat_euro_pain_180_canonical
#   ),
#   prior_check_euro_pain_365 = f_prior_predictive_check_vent(
#     m_prior_neutral_euro_pain_365_canonical,
#     m_prior_left_euro_pain_365_canonical,
#     m_prior_right_euro_pain_365_canonical,
#     m_prior_flat_euro_pain_365_canonical
#   ),
#   prior_visual_euro_pain_365_neutral = prediction_visual(
#     m_prior_neutral_euro_pain_365_canonical
#   ),
#   prior_visual_euro_pain_365_left = prediction_visual(
#     m_prior_left_euro_pain_365_canonical
#   ),
#   prior_visual_euro_pain_365_right = prediction_visual(
#     m_prior_right_euro_pain_365_canonical
#   ),
#   prior_visual_euro_pain_365_flat = prediction_visual(
#     m_prior_flat_euro_pain_365_canonical
#   ),
#   prior_check_euro_anxiety_90 = f_prior_predictive_check_vent(
#     m_prior_neutral_euro_anxiety_90_canonical,
#     m_prior_left_euro_anxiety_90_canonical,
#     m_prior_right_euro_anxiety_90_canonical,
#     m_prior_flat_euro_anxiety_90_canonical
#   ),
#   prior_visual_euro_anxiety_90_neutral = prediction_visual(
#     m_prior_neutral_euro_anxiety_90_canonical
#   ),
#   prior_visual_euro_anxiety_90_left = prediction_visual(
#     m_prior_left_euro_anxiety_90_canonical
#   ),
#   prior_visual_euro_anxiety_90_right = prediction_visual(
#     m_prior_right_euro_anxiety_90_canonical
#   ),
#   prior_visual_euro_anxiety_90_flat = prediction_visual(
#     m_prior_flat_euro_anxiety_90_canonical
#   ),
#   prior_check_euro_anxiety_180 = f_prior_predictive_check_vent(
#     m_prior_neutral_euro_anxiety_180_canonical,
#     m_prior_left_euro_anxiety_180_canonical,
#     m_prior_right_euro_anxiety_180_canonical,
#     m_prior_flat_euro_anxiety_180_canonical
#   ),
#   prior_visual_euro_anxiety_180_neutral = prediction_visual(
#     m_prior_neutral_euro_anxiety_180_canonical
#   ),
#   prior_visual_euro_anxiety_180_left = prediction_visual(
#     m_prior_left_euro_anxiety_180_canonical
#   ),
#   prior_visual_euro_anxiety_180_right = prediction_visual(
#     m_prior_right_euro_anxiety_180_canonical
#   ),
#   prior_visual_euro_anxiety_180_flat = prediction_visual(
#     m_prior_flat_euro_anxiety_180_canonical
#   ),
#   prior_check_euro_anxiety_365 = f_prior_predictive_check_vent(
#     m_prior_neutral_euro_anxiety_365_canonical,
#     m_prior_left_euro_anxiety_365_canonical,
#     m_prior_right_euro_anxiety_365_canonical,
#     m_prior_flat_euro_anxiety_365_canonical
#   ),
#   prior_visual_euro_anxiety_365_neutral = prediction_visual(
#     m_prior_neutral_euro_anxiety_365_canonical
#   ),
#   prior_visual_euro_anxiety_365_left = prediction_visual(
#     m_prior_left_euro_anxiety_365_canonical
#   ),
#   prior_visual_euro_anxiety_365_right = prediction_visual(
#     m_prior_right_euro_anxiety_365_canonical
#   ),
#   prior_visual_euro_anxiety_365_flat = prediction_visual(
#     m_prior_flat_euro_anxiety_365_canonical
#   ),

#   ### Posterior Simulation ----
#   m_posterior_mrs_90_minimal = f_posterior_mrs_90_minimal(ich_aggressive),
#   m_posterior_neutral_mrs_90_canonical = f_posterior_neutral_mrs_90_canonical(
#     ich_aggressive
#   ),
#   m_posterior_left_mrs_90_canonical = f_posterior_left_mrs_90_canonical(
#     ich_aggressive
#   ),
#   m_posterior_right_mrs_90_canonical = f_posterior_right_mrs_90_canonical(
#     ich_aggressive
#   ),
#   m_posterior_flat_mrs_90_canonical = f_posterior_flat_mrs_90_canonical(
#     ich_aggressive
#   ),
#   m_posterior_mrs_180_minimal = f_posterior_mrs_180_minimal(ich_aggressive),
#   m_posterior_neutral_mrs_180_canonical = f_posterior_neutral_mrs_180_canonical(
#     ich_aggressive
#   ),
#   m_posterior_left_mrs_180_canonical = f_posterior_left_mrs_180_canonical(
#     ich_aggressive
#   ),
#   m_posterior_right_mrs_180_canonical = f_posterior_right_mrs_180_canonical(
#     ich_aggressive
#   ),
#   m_posterior_flat_mrs_180_canonical = f_posterior_flat_mrs_180_canonical(
#     ich_aggressive
#   ),
#   m_posterior_mrs_365_minimal = f_posterior_mrs_365_minimal(ich_aggressive),
#   m_posterior_neutral_mrs_365_canonical = f_posterior_neutral_mrs_365_canonical(
#     ich_aggressive
#   ),
#   m_posterior_left_mrs_365_canonical = f_posterior_left_mrs_365_canonical(
#     ich_aggressive
#   ),
#   m_posterior_right_mrs_365_canonical = f_posterior_right_mrs_365_canonical(
#     ich_aggressive
#   ),
#   m_posterior_flat_mrs_365_canonical = f_posterior_flat_mrs_365_canonical(
#     ich_aggressive
#   ),
#   m_posterior_neutral_euro_mobility_90_canonical = f_posterior_neutral_euro_mobility_90_canonical(
#     ich_aggressive
#   ),
#   m_posterior_left_euro_mobility_90_canonical = f_posterior_left_euro_mobility_90_canonical(
#     ich_aggressive
#   ),
#   m_posterior_right_euro_mobility_90_canonical = f_posterior_right_euro_mobility_90_canonical(
#     ich_aggressive
#   ),
#   m_posterior_flat_euro_mobility_90_canonical = f_posterior_flat_euro_mobility_90_canonical(
#     ich_aggressive
#   ),
#   m_posterior_neutral_euro_mobility_180_canonical = f_posterior_neutral_euro_mobility_180_canonical(
#     ich_aggressive
#   ),
#   m_posterior_left_euro_mobility_180_canonical = f_posterior_left_euro_mobility_180_canonical(
#     ich_aggressive
#   ),
#   m_posterior_right_euro_mobility_180_canonical = f_posterior_right_euro_mobility_180_canonical(
#     ich_aggressive
#   ),
#   m_posterior_flat_euro_mobility_180_canonical = f_posterior_flat_euro_mobility_180_canonical(
#     ich_aggressive
#   ),
#   m_posterior_neutral_euro_mobility_365_canonical = f_posterior_neutral_euro_mobility_365_canonical(
#     ich_aggressive
#   ),
#   m_posterior_left_euro_mobility_365_canonical = f_posterior_left_euro_mobility_365_canonical(
#     ich_aggressive
#   ),
#   m_posterior_right_euro_mobility_365_canonical = f_posterior_right_euro_mobility_365_canonical(
#     ich_aggressive
#   ),
#   m_posterior_flat_euro_mobility_365_canonical = f_posterior_flat_euro_mobility_365_canonical(
#     ich_aggressive
#   ),
#   m_posterior_neutral_euro_selfcare_90_canonical = f_posterior_neutral_euro_selfcare_90_canonical(
#     ich_aggressive
#   ),
#   m_posterior_left_euro_selfcare_90_canonical = f_posterior_left_euro_selfcare_90_canonical(
#     ich_aggressive
#   ),
#   m_posterior_right_euro_selfcare_90_canonical = f_posterior_right_euro_selfcare_90_canonical(
#     ich_aggressive
#   ),
#   m_posterior_flat_euro_selfcare_90_canonical = f_posterior_flat_euro_selfcare_90_canonical(
#     ich_aggressive
#   ),
#   m_posterior_neutral_euro_selfcare_180_canonical = f_posterior_neutral_euro_selfcare_180_canonical(
#     ich_aggressive
#   ),
#   m_posterior_left_euro_selfcare_180_canonical = f_posterior_left_euro_selfcare_180_canonical(
#     ich_aggressive
#   ),
#   m_posterior_right_euro_selfcare_180_canonical = f_posterior_right_euro_selfcare_180_canonical(
#     ich_aggressive
#   ),
#   m_posterior_flat_euro_selfcare_180_canonical = f_posterior_flat_euro_selfcare_180_canonical(
#     ich_aggressive
#   ),
#   m_posterior_neutral_euro_selfcare_365_canonical = f_posterior_neutral_euro_selfcare_365_canonical(
#     ich_aggressive
#   ),
#   m_posterior_left_euro_selfcare_365_canonical = f_posterior_left_euro_selfcare_365_canonical(
#     ich_aggressive
#   ),
#   m_posterior_right_euro_selfcare_365_canonical = f_posterior_right_euro_selfcare_365_canonical(
#     ich_aggressive
#   ),
#   m_posterior_flat_euro_selfcare_365_canonical = f_posterior_flat_euro_selfcare_365_canonical(
#     ich_aggressive
#   ),
#   m_posterior_neutral_euro_usual_90_canonical = f_posterior_neutral_euro_usual_90_canonical(
#     ich_aggressive
#   ),
#   m_posterior_left_euro_usual_90_canonical = f_posterior_left_euro_usual_90_canonical(
#     ich_aggressive
#   ),
#   m_posterior_right_euro_usual_90_canonical = f_posterior_right_euro_usual_90_canonical(
#     ich_aggressive
#   ),
#   m_posterior_flat_euro_usual_90_canonical = f_posterior_flat_euro_usual_90_canonical(
#     ich_aggressive
#   ),
#   m_posterior_neutral_euro_usual_180_canonical = f_posterior_neutral_euro_usual_180_canonical(
#     ich_aggressive
#   ),
#   m_posterior_left_euro_usual_180_canonical = f_posterior_left_euro_usual_180_canonical(
#     ich_aggressive
#   ),
#   m_posterior_right_euro_usual_180_canonical = f_posterior_right_euro_usual_180_canonical(
#     ich_aggressive
#   ),
#   m_posterior_flat_euro_usual_180_canonical = f_posterior_flat_euro_usual_180_canonical(
#     ich_aggressive
#   ),
#   m_posterior_neutral_euro_usual_365_canonical = f_posterior_neutral_euro_usual_365_canonical(
#     ich_aggressive
#   ),
#   m_posterior_left_euro_usual_365_canonical = f_posterior_left_euro_usual_365_canonical(
#     ich_aggressive
#   ),
#   m_posterior_right_euro_usual_365_canonical = f_posterior_right_euro_usual_365_canonical(
#     ich_aggressive
#   ),
#   m_posterior_flat_euro_usual_365_canonical = f_posterior_flat_euro_usual_365_canonical(
#     ich_aggressive
#   ),
#   m_posterior_neutral_euro_pain_90_canonical = f_posterior_neutral_euro_pain_90_canonical(
#     ich_aggressive
#   ),
#   m_posterior_left_euro_pain_90_canonical = f_posterior_left_euro_pain_90_canonical(
#     ich_aggressive
#   ),
#   m_posterior_right_euro_pain_90_canonical = f_posterior_right_euro_pain_90_canonical(
#     ich_aggressive
#   ),
#   m_posterior_flat_euro_pain_90_canonical = f_posterior_flat_euro_pain_90_canonical(
#     ich_aggressive
#   ),
#   m_posterior_neutral_euro_pain_180_canonical = f_posterior_neutral_euro_pain_180_canonical(
#     ich_aggressive
#   ),
#   m_posterior_left_euro_pain_180_canonical = f_posterior_left_euro_pain_180_canonical(
#     ich_aggressive
#   ),
#   m_posterior_right_euro_pain_180_canonical = f_posterior_right_euro_pain_180_canonical(
#     ich_aggressive
#   ),
#   m_posterior_flat_euro_pain_180_canonical = f_posterior_flat_euro_pain_180_canonical(
#     ich_aggressive
#   ),
#   m_posterior_neutral_euro_pain_365_canonical = f_posterior_neutral_euro_pain_365_canonical(
#     ich_aggressive
#   ),
#   m_posterior_left_euro_pain_365_canonical = f_posterior_left_euro_pain_365_canonical(
#     ich_aggressive
#   ),
#   m_posterior_right_euro_pain_365_canonical = f_posterior_right_euro_pain_365_canonical(
#     ich_aggressive
#   ),
#   m_posterior_flat_euro_pain_365_canonical = f_posterior_flat_euro_pain_365_canonical(
#     ich_aggressive
#   ),
#   m_posterior_neutral_euro_anxiety_90_canonical = f_posterior_neutral_euro_anxiety_90_canonical(
#     ich_aggressive
#   ),
#   m_posterior_left_euro_anxiety_90_canonical = f_posterior_left_euro_anxiety_90_canonical(
#     ich_aggressive
#   ),
#   m_posterior_right_euro_anxiety_90_canonical = f_posterior_right_euro_anxiety_90_canonical(
#     ich_aggressive
#   ),
#   m_posterior_flat_euro_anxiety_90_canonical = f_posterior_flat_euro_anxiety_90_canonical(
#     ich_aggressive
#   ),
#   m_posterior_neutral_euro_anxiety_180_canonical = f_posterior_neutral_euro_anxiety_180_canonical(
#     ich_aggressive
#   ),
#   m_posterior_left_euro_anxiety_180_canonical = f_posterior_left_euro_anxiety_180_canonical(
#     ich_aggressive
#   ),
#   m_posterior_right_euro_anxiety_180_canonical = f_posterior_right_euro_anxiety_180_canonical(
#     ich_aggressive
#   ),
#   m_posterior_flat_euro_anxiety_180_canonical = f_posterior_flat_euro_anxiety_180_canonical(
#     ich_aggressive
#   ),
#   m_posterior_neutral_euro_anxiety_365_canonical = f_posterior_neutral_euro_anxiety_365_canonical(
#     ich_aggressive
#   ),
#   m_posterior_left_euro_anxiety_365_canonical = f_posterior_left_euro_anxiety_365_canonical(
#     ich_aggressive
#   ),
#   m_posterior_right_euro_anxiety_365_canonical = f_posterior_right_euro_anxiety_365_canonical(
#     ich_aggressive
#   ),
#   m_posterior_flat_euro_anxiety_365_canonical = f_posterior_flat_euro_anxiety_365_canonical(
#     ich_aggressive
#   ),
#   m_vas_90 = f_vas_90_zoib(ich_aggressive),
#   m_vas_180 = f_vas_180_beta(vas_data_transformed),
#   m_vas_365 = f_vas_365_beta(vas_data_transformed),

#   #### Posterior Diagnostics

#   diag_posterior_mrs_90_minimal = posterior_diagnostics(
#     m_posterior_mrs_90_minimal
#   ),
#   diag_posterior_neutral_mrs_90_canonical = posterior_diagnostics(
#     m_posterior_neutral_mrs_90_canonical
#   ),
#   diag_posterior_left_mrs_90_canonical = posterior_diagnostics(
#     m_posterior_left_mrs_90_canonical
#   ),
#   diag_posterior_right_mrs_90_canonical = posterior_diagnostics(
#     m_posterior_right_mrs_90_canonical
#   ),
#   diag_posterior_flat_mrs_90_canonical = posterior_diagnostics(
#     m_posterior_flat_mrs_90_canonical
#   ),
#   diag_posterior_mrs_180_minimal = posterior_diagnostics(
#     m_posterior_mrs_180_minimal
#   ),
#   diag_posterior_neutral_mrs_180_canonical = posterior_diagnostics(
#     m_posterior_neutral_mrs_180_canonical
#   ),
#   diag_posterior_left_mrs_180_canonical = posterior_diagnostics(
#     m_posterior_left_mrs_180_canonical
#   ),
#   diag_posterior_right_mrs_180_canonical = posterior_diagnostics(
#     m_posterior_right_mrs_180_canonical
#   ),
#   diag_posterior_flat_mrs_180_canonical = posterior_diagnostics(
#     m_posterior_flat_mrs_180_canonical
#   ),
#   diag_posterior_mrs_365_minimal = posterior_diagnostics(
#     m_posterior_mrs_365_minimal
#   ),
#   diag_posterior_neutral_mrs_365_canonical = posterior_diagnostics(
#     m_posterior_neutral_mrs_365_canonical
#   ),
#   diag_posterior_left_mrs_365_canonical = posterior_diagnostics(
#     m_posterior_left_mrs_365_canonical
#   ),
#   diag_posterior_right_mrs_365_canonical = posterior_diagnostics(
#     m_posterior_right_mrs_365_canonical
#   ),
#   diag_posterior_flat_mrs_365_canonical = posterior_diagnostics(
#     m_posterior_flat_mrs_365_canonical
#   ),
#   diag_posterior_neutral_euro_mobility_90_canonical = posterior_diagnostics(
#     m_posterior_neutral_euro_mobility_90_canonical
#   ),
#   diag_posterior_left_euro_mobility_90_canonical = posterior_diagnostics(
#     m_posterior_left_euro_mobility_90_canonical
#   ),
#   diag_posterior_right_euro_mobility_90_canonical = posterior_diagnostics(
#     m_posterior_right_euro_mobility_90_canonical
#   ),
#   diag_posterior_flat_euro_mobility_90_canonical = posterior_diagnostics(
#     m_posterior_flat_euro_mobility_90_canonical
#   ),
#   diag_posterior_neutral_euro_mobility_180_canonical = posterior_diagnostics(
#     m_posterior_neutral_euro_mobility_180_canonical
#   ),
#   diag_posterior_left_euro_mobility_180_canonical = posterior_diagnostics(
#     m_posterior_left_euro_mobility_180_canonical
#   ),
#   diag_posterior_right_euro_mobility_180_canonical = posterior_diagnostics(
#     m_posterior_right_euro_mobility_180_canonical
#   ),
#   diag_posterior_flat_euro_mobility_180_canonical = posterior_diagnostics(
#     m_posterior_flat_euro_mobility_180_canonical
#   ),
#   diag_posterior_neutral_euro_mobility_365_canonical = posterior_diagnostics(
#     m_posterior_neutral_euro_mobility_365_canonical
#   ),
#   diag_posterior_left_euro_mobility_365_canonical = posterior_diagnostics(
#     m_posterior_left_euro_mobility_365_canonical
#   ),
#   diag_posterior_right_euro_mobility_365_canonical = posterior_diagnostics(
#     m_posterior_right_euro_mobility_365_canonical
#   ),
#   diag_posterior_flat_euro_mobility_365_canonical = posterior_diagnostics(
#     m_posterior_flat_euro_mobility_365_canonical
#   ),
#   diag_posterior_neutral_euro_selfcare_90_canonical = posterior_diagnostics(
#     m_posterior_neutral_euro_selfcare_90_canonical
#   ),
#   diag_posterior_left_euro_selfcare_90_canonical = posterior_diagnostics(
#     m_posterior_left_euro_selfcare_90_canonical
#   ),
#   diag_posterior_right_euro_selfcare_90_canonical = posterior_diagnostics(
#     m_posterior_right_euro_selfcare_90_canonical
#   ),
#   diag_posterior_flat_euro_selfcare_90_canonical = posterior_diagnostics(
#     m_posterior_flat_euro_selfcare_90_canonical
#   ),
#   diag_posterior_neutral_euro_selfcare_180_canonical = posterior_diagnostics(
#     m_posterior_neutral_euro_selfcare_180_canonical
#   ),
#   diag_posterior_left_euro_selfcare_180_canonical = posterior_diagnostics(
#     m_posterior_left_euro_selfcare_180_canonical
#   ),
#   diag_posterior_right_euro_selfcare_180_canonical = posterior_diagnostics(
#     m_posterior_right_euro_selfcare_180_canonical
#   ),
#   diag_posterior_flat_euro_selfcare_180_canonical = posterior_diagnostics(
#     m_posterior_flat_euro_selfcare_180_canonical
#   ),
#   diag_posterior_neutral_euro_selfcare_365_canonical = posterior_diagnostics(
#     m_posterior_neutral_euro_selfcare_365_canonical
#   ),
#   diag_posterior_left_euro_selfcare_365_canonical = posterior_diagnostics(
#     m_posterior_left_euro_selfcare_365_canonical
#   ),
#   diag_posterior_right_euro_selfcare_365_canonical = posterior_diagnostics(
#     m_posterior_right_euro_selfcare_365_canonical
#   ),
#   diag_posterior_flat_euro_selfcare_365_canonical = posterior_diagnostics(
#     m_posterior_flat_euro_selfcare_365_canonical
#   ),
#   diag_posterior_neutral_euro_usual_90_canonical = posterior_diagnostics(
#     m_posterior_neutral_euro_usual_90_canonical
#   ),
#   diag_posterior_left_euro_usual_90_canonical = posterior_diagnostics(
#     m_posterior_left_euro_usual_90_canonical
#   ),
#   diag_posterior_right_euro_usual_90_canonical = posterior_diagnostics(
#     m_posterior_right_euro_usual_90_canonical
#   ),
#   diag_posterior_flat_euro_usual_90_canonical = posterior_diagnostics(
#     m_posterior_flat_euro_usual_90_canonical
#   ),
#   diag_posterior_neutral_euro_usual_180_canonical = posterior_diagnostics(
#     m_posterior_neutral_euro_usual_180_canonical
#   ),
#   diag_posterior_left_euro_usual_180_canonical = posterior_diagnostics(
#     m_posterior_left_euro_usual_180_canonical
#   ),
#   diag_posterior_right_euro_usual_180_canonical = posterior_diagnostics(
#     m_posterior_right_euro_usual_180_canonical
#   ),
#   diag_posterior_flat_euro_usual_180_canonical = posterior_diagnostics(
#     m_posterior_flat_euro_usual_180_canonical
#   ),
#   diag_posterior_neutral_euro_usual_365_canonical = posterior_diagnostics(
#     m_posterior_neutral_euro_usual_365_canonical
#   ),
#   diag_posterior_left_euro_usual_365_canonical = posterior_diagnostics(
#     m_posterior_left_euro_usual_365_canonical
#   ),
#   diag_posterior_right_euro_usual_365_canonical = posterior_diagnostics(
#     m_posterior_right_euro_usual_365_canonical
#   ),
#   diag_posterior_flat_euro_usual_365_canonical = posterior_diagnostics(
#     m_posterior_flat_euro_usual_365_canonical
#   ),
#   diag_posterior_neutral_euro_pain_90_canonical = posterior_diagnostics(
#     m_posterior_neutral_euro_pain_90_canonical
#   ),
#   diag_posterior_left_euro_pain_90_canonical = posterior_diagnostics(
#     m_posterior_left_euro_pain_90_canonical
#   ),
#   diag_posterior_right_euro_pain_90_canonical = posterior_diagnostics(
#     m_posterior_right_euro_pain_90_canonical
#   ),
#   diag_posterior_flat_euro_pain_90_canonical = posterior_diagnostics(
#     m_posterior_flat_euro_pain_90_canonical
#   ),
#   diag_posterior_neutral_euro_pain_180_canonical = posterior_diagnostics(
#     m_posterior_neutral_euro_pain_180_canonical
#   ),
#   diag_posterior_left_euro_pain_180_canonical = posterior_diagnostics(
#     m_posterior_left_euro_pain_180_canonical
#   ),
#   diag_posterior_right_euro_pain_180_canonical = posterior_diagnostics(
#     m_posterior_right_euro_pain_180_canonical
#   ),
#   diag_posterior_flat_euro_pain_180_canonical = posterior_diagnostics(
#     m_posterior_flat_euro_pain_180_canonical
#   ),
#   diag_posterior_neutral_euro_pain_365_canonical = posterior_diagnostics(
#     m_posterior_neutral_euro_pain_365_canonical
#   ),
#   diag_posterior_left_euro_pain_365_canonical = posterior_diagnostics(
#     m_posterior_left_euro_pain_365_canonical
#   ),
#   diag_posterior_right_euro_pain_365_canonical = posterior_diagnostics(
#     m_posterior_right_euro_pain_365_canonical
#   ),
#   diag_posterior_flat_euro_pain_365_canonical = posterior_diagnostics(
#     m_posterior_flat_euro_pain_365_canonical
#   ),
#   diag_posterior_neutral_euro_anxiety_90_canonical = posterior_diagnostics(
#     m_posterior_neutral_euro_anxiety_90_canonical
#   ),
#   diag_posterior_left_euro_anxiety_90_canonical = posterior_diagnostics(
#     m_posterior_left_euro_anxiety_90_canonical
#   ),
#   diag_posterior_right_euro_anxiety_90_canonical = posterior_diagnostics(
#     m_posterior_right_euro_anxiety_90_canonical
#   ),
#   diag_posterior_flat_euro_anxiety_90_canonical = posterior_diagnostics(
#     m_posterior_flat_euro_anxiety_90_canonical
#   ),
#   diag_posterior_neutral_euro_anxiety_180_canonical = posterior_diagnostics(
#     m_posterior_neutral_euro_anxiety_180_canonical
#   ),
#   diag_posterior_left_euro_anxiety_180_canonical = posterior_diagnostics(
#     m_posterior_left_euro_anxiety_180_canonical
#   ),
#   diag_posterior_right_euro_anxiety_180_canonical = posterior_diagnostics(
#     m_posterior_right_euro_anxiety_180_canonical
#   ),
#   diag_posterior_flat_euro_anxiety_180_canonical = posterior_diagnostics(
#     m_posterior_flat_euro_anxiety_180_canonical
#   ),
#   diag_posterior_neutral_euro_anxiety_365_canonical = posterior_diagnostics(
#     m_posterior_neutral_euro_anxiety_365_canonical
#   ),
#   diag_posterior_left_euro_anxiety_365_canonical = posterior_diagnostics(
#     m_posterior_left_euro_anxiety_365_canonical
#   ),
#   diag_posterior_right_euro_anxiety_365_canonical = posterior_diagnostics(
#     m_posterior_right_euro_anxiety_365_canonical
#   ),
#   diag_posterior_flat_euro_anxiety_365_canonical = posterior_diagnostics(
#     m_posterior_flat_euro_anxiety_365_canonical
#   ),
#   diag_posterior_vas_90 = posterior_diagnostics(m_vas_90),
#   diag_posterior_vas_180 = posterior_diagnostics(m_vas_180),
#   diag_posterior_vas_365 = posterior_diagnostics(m_vas_365),

#   #### Posterior Predictive Checks
#   post_pred_check_mrs_90_minimal = f_post_predictive_check(
#     m_posterior_mrs_90_minimal
#   ),
#   post_pred_check_neutral_mrs_90_canonical = f_post_predictive_check(
#     m_posterior_neutral_mrs_90_canonical
#   ),
#   post_pred_check_left_mrs_90_canonical = f_post_predictive_check(
#     m_posterior_left_mrs_90_canonical
#   ),
#   post_pred_check_right_mrs_90_canonical = f_post_predictive_check(
#     m_posterior_right_mrs_90_canonical
#   ),
#   post_pred_check_flat_mrs_90_canonical = f_post_predictive_check(
#     m_posterior_flat_mrs_90_canonical
#   ),
#   post_pred_check_mrs_180_minimal = f_post_predictive_check(
#     m_posterior_mrs_180_minimal
#   ),
#   post_pred_check_neutral_mrs_180_canonical = f_post_predictive_check(
#     m_posterior_neutral_mrs_180_canonical
#   ),
#   post_pred_check_left_mrs_180_canonical = f_post_predictive_check(
#     m_posterior_left_mrs_180_canonical
#   ),
#   post_pred_check_right_mrs_180_canonical = f_post_predictive_check(
#     m_posterior_right_mrs_180_canonical
#   ),
#   post_pred_check_flat_mrs_180_canonical = f_post_predictive_check(
#     m_posterior_flat_mrs_180_canonical
#   ),
#   post_pred_check_mrs_365_minimal = f_post_predictive_check(
#     m_posterior_mrs_365_minimal
#   ),
#   post_pred_check_neutral_mrs_365_canonical = f_post_predictive_check(
#     m_posterior_neutral_mrs_365_canonical
#   ),
#   post_pred_check_left_mrs_365_canonical = f_post_predictive_check(
#     m_posterior_left_mrs_365_canonical
#   ),
#   post_pred_check_right_mrs_365_canonical = f_post_predictive_check(
#     m_posterior_right_mrs_365_canonical
#   ),
#   post_pred_check_flat_mrs_365_canonical = f_post_predictive_check(
#     m_posterior_flat_mrs_365_canonical
#   ),
#   post_pred_check_neutral_euro_mobility_90_canonical = f_post_predictive_check(
#     m_posterior_neutral_euro_mobility_90_canonical
#   ),
#   post_pred_check_left_euro_mobility_90_canonical = f_post_predictive_check(
#     m_posterior_left_euro_mobility_90_canonical
#   ),
#   post_pred_check_right_euro_mobility_90_canonical = f_post_predictive_check(
#     m_posterior_right_euro_mobility_90_canonical
#   ),
#   post_pred_check_flat_euro_mobility_90_canonical = f_post_predictive_check(
#     m_posterior_flat_euro_mobility_90_canonical
#   ),
#   post_pred_check_neutral_euro_mobility_180_canonical = f_post_predictive_check(
#     m_posterior_neutral_euro_mobility_180_canonical
#   ),
#   post_pred_check_left_euro_mobility_180_canonical = f_post_predictive_check(
#     m_posterior_left_euro_mobility_180_canonical
#   ),
#   post_pred_check_right_euro_mobility_180_canonical = f_post_predictive_check(
#     m_posterior_right_euro_mobility_180_canonical
#   ),
#   post_pred_check_flat_euro_mobility_180_canonical = f_post_predictive_check(
#     m_posterior_flat_euro_mobility_180_canonical
#   ),
#   post_pred_check_neutral_euro_mobility_365_canonical = f_post_predictive_check(
#     m_posterior_neutral_euro_mobility_365_canonical
#   ),
#   post_pred_check_left_euro_mobility_365_canonical = f_post_predictive_check(
#     m_posterior_left_euro_mobility_365_canonical
#   ),
#   post_pred_check_right_euro_mobility_365_canonical = f_post_predictive_check(
#     m_posterior_right_euro_mobility_365_canonical
#   ),
#   post_pred_check_flat_euro_mobility_365_canonical = f_post_predictive_check(
#     m_posterior_flat_euro_mobility_365_canonical
#   ),
#   post_pred_check_neutral_euro_selfcare_90_canonical = f_post_predictive_check(
#     m_posterior_neutral_euro_selfcare_90_canonical
#   ),
#   post_pred_check_left_euro_selfcare_90_canonical = f_post_predictive_check(
#     m_posterior_left_euro_selfcare_90_canonical
#   ),
#   post_pred_check_right_euro_selfcare_90_canonical = f_post_predictive_check(
#     m_posterior_right_euro_selfcare_90_canonical
#   ),
#   post_pred_check_flat_euro_selfcare_90_canonical = f_post_predictive_check(
#     m_posterior_flat_euro_selfcare_90_canonical
#   ),
#   post_pred_check_neutral_euro_selfcare_180_canonical = f_post_predictive_check(
#     m_posterior_neutral_euro_selfcare_180_canonical
#   ),
#   post_pred_check_left_euro_selfcare_180_canonical = f_post_predictive_check(
#     m_posterior_left_euro_selfcare_180_canonical
#   ),
#   post_pred_check_right_euro_selfcare_180_canonical = f_post_predictive_check(
#     m_posterior_right_euro_selfcare_180_canonical
#   ),
#   post_pred_check_flat_euro_selfcare_180_canonical = f_post_predictive_check(
#     m_posterior_flat_euro_selfcare_180_canonical
#   ),
#   post_pred_check_neutral_euro_selfcare_365_canonical = f_post_predictive_check(
#     m_posterior_neutral_euro_selfcare_365_canonical
#   ),
#   post_pred_check_left_euro_selfcare_365_canonical = f_post_predictive_check(
#     m_posterior_left_euro_selfcare_365_canonical
#   ),
#   post_pred_check_right_euro_selfcare_365_canonical = f_post_predictive_check(
#     m_posterior_right_euro_selfcare_365_canonical
#   ),
#   post_pred_check_flat_euro_selfcare_365_canonical = f_post_predictive_check(
#     m_posterior_flat_euro_selfcare_365_canonical
#   ),
#   post_pred_check_neutral_euro_usual_90_canonical = f_post_predictive_check(
#     m_posterior_neutral_euro_usual_90_canonical
#   ),
#   post_pred_check_left_euro_usual_90_canonical = f_post_predictive_check(
#     m_posterior_left_euro_usual_90_canonical
#   ),
#   post_pred_check_right_euro_usual_90_canonical = f_post_predictive_check(
#     m_posterior_right_euro_usual_90_canonical
#   ),
#   post_pred_check_flat_euro_usual_90_canonical = f_post_predictive_check(
#     m_posterior_flat_euro_usual_90_canonical
#   ),
#   post_pred_check_neutral_euro_usual_180_canonical = f_post_predictive_check(
#     m_posterior_neutral_euro_usual_180_canonical
#   ),
#   post_pred_check_left_euro_usual_180_canonical = f_post_predictive_check(
#     m_posterior_left_euro_usual_180_canonical
#   ),
#   post_pred_check_right_euro_usual_180_canonical = f_post_predictive_check(
#     m_posterior_right_euro_usual_180_canonical
#   ),
#   post_pred_check_flat_euro_usual_180_canonical = f_post_predictive_check(
#     m_posterior_flat_euro_usual_180_canonical
#   ),
#   post_pred_check_neutral_euro_usual_365_canonical = f_post_predictive_check(
#     m_posterior_neutral_euro_usual_365_canonical
#   ),
#   post_pred_check_left_euro_usual_365_canonical = f_post_predictive_check(
#     m_posterior_left_euro_usual_365_canonical
#   ),
#   post_pred_check_right_euro_usual_365_canonical = f_post_predictive_check(
#     m_posterior_right_euro_usual_365_canonical
#   ),
#   post_pred_check_flat_euro_usual_365_canonical = f_post_predictive_check(
#     m_posterior_flat_euro_usual_365_canonical
#   ),
#   post_pred_check_neutral_euro_pain_90_canonical = f_post_predictive_check(
#     m_posterior_neutral_euro_pain_90_canonical
#   ),
#   post_pred_check_left_euro_pain_90_canonical = f_post_predictive_check(
#     m_posterior_left_euro_pain_90_canonical
#   ),
#   post_pred_check_right_euro_pain_90_canonical = f_post_predictive_check(
#     m_posterior_right_euro_pain_90_canonical
#   ),
#   post_pred_check_flat_euro_pain_90_canonical = f_post_predictive_check(
#     m_posterior_flat_euro_pain_90_canonical
#   ),
#   post_pred_check_neutral_euro_pain_180_canonical = f_post_predictive_check(
#     m_posterior_neutral_euro_pain_180_canonical
#   ),
#   post_pred_check_left_euro_pain_180_canonical = f_post_predictive_check(
#     m_posterior_left_euro_pain_180_canonical
#   ),
#   post_pred_check_right_euro_pain_180_canonical = f_post_predictive_check(
#     m_posterior_right_euro_pain_180_canonical
#   ),
#   post_pred_check_flat_euro_pain_180_canonical = f_post_predictive_check(
#     m_posterior_flat_euro_pain_180_canonical
#   ),
#   post_pred_check_neutral_euro_pain_365_canonical = f_post_predictive_check(
#     m_posterior_neutral_euro_pain_365_canonical
#   ),
#   post_pred_check_left_euro_pain_365_canonical = f_post_predictive_check(
#     m_posterior_left_euro_pain_365_canonical
#   ),
#   post_pred_check_right_euro_pain_365_canonical = f_post_predictive_check(
#     m_posterior_right_euro_pain_365_canonical
#   ),
#   post_pred_check_flat_euro_pain_365_canonical = f_post_predictive_check(
#     m_posterior_flat_euro_pain_365_canonical
#   ),
#   post_pred_check_neutral_euro_anxiety_90_canonical = f_post_predictive_check(
#     m_posterior_neutral_euro_anxiety_90_canonical
#   ),
#   post_pred_check_left_euro_anxiety_90_canonical = f_post_predictive_check(
#     m_posterior_left_euro_anxiety_90_canonical
#   ),
#   post_pred_check_right_euro_anxiety_90_canonical = f_post_predictive_check(
#     m_posterior_right_euro_anxiety_90_canonical
#   ),
#   post_pred_check_flat_euro_anxiety_90_canonical = f_post_predictive_check(
#     m_posterior_flat_euro_anxiety_90_canonical
#   ),
#   post_pred_check_neutral_euro_anxiety_180_canonical = f_post_predictive_check(
#     m_posterior_neutral_euro_anxiety_180_canonical
#   ),
#   post_pred_check_left_euro_anxiety_180_canonical = f_post_predictive_check(
#     m_posterior_left_euro_anxiety_180_canonical
#   ),
#   post_pred_check_right_euro_anxiety_180_canonical = f_post_predictive_check(
#     m_posterior_right_euro_anxiety_180_canonical
#   ),
#   post_pred_check_flat_euro_anxiety_180_canonical = f_post_predictive_check(
#     m_posterior_flat_euro_anxiety_180_canonical
#   ),
#   post_pred_check_neutral_euro_anxiety_365_canonical = f_post_predictive_check(
#     m_posterior_neutral_euro_anxiety_365_canonical
#   ),
#   post_pred_check_left_euro_anxiety_365_canonical = f_post_predictive_check(
#     m_posterior_left_euro_anxiety_365_canonical
#   ),
#   post_pred_check_right_euro_anxiety_365_canonical = f_post_predictive_check(
#     m_posterior_right_euro_anxiety_365_canonical
#   ),
#   post_pred_check_flat_euro_anxiety_365_canonical = f_post_predictive_check(
#     m_posterior_flat_euro_anxiety_365_canonical
#   ),
#   post_pred_check_vas_90 = f_post_predictive_check_vas(m_vas_90),
#   post_pred_check_vas_180 = f_post_predictive_check_vas(m_vas_180),
#   post_pred_check_vas_365 = f_post_predictive_check_vas(m_vas_365),

#   ## Subgroup Analyses ----
#   subgroup_location = f_subgroup_by_location(ich_aggressive),
#   subgroup_study = f_subgroup_by_study(ich_aggressive),
#   subgroup_table = f_subgroup_table(subgroup_location, subgroup_study),
#   table3_docx = gtsave(subgroup_table, here("manuscripts", "table3.docx")),

#   ## (Further) Sensitivity Analyses
#   m_posterior_minimal_neurosurgery = f_posterior_minimal_neurosurgery(
#     ich_aggressive
#   ),
#   m_posterior_alternative1_neurosurgery = f_posterior_alternative1_neurosurgery(
#     ich_aggressive
#   ),
#   m_posterior_alternative2_neurosurgery = f_posterior_alternative2_neurosurgery(
#     ich_aggressive
#   ),
#   m_posterior_alternative3_neurosurgery = f_posterior_alternative3_neurosurgery(
#     ich_aggressive
#   ),
#   m_posterior_alternative4_neurosurgery = f_posterior_alternative4_neurosurgery(
#     ich_aggressive
#   ),
#   m_posterior_minimal_evd = f_posterior_minimal_neurosurgery(
#     ich_aggressive
#   ),
#   m_posterior_alternative1_evd = f_posterior_alternative1_evd(
#     ich_aggressive
#   ),
#   m_posterior_alternative2_evd = f_posterior_alternative2_evd(
#     ich_aggressive
#   ),
#   m_posterior_alternative3_evd = f_posterior_alternative3_evd(
#     ich_aggressive
#   ),
#   m_posterior_alternative4_evd = f_posterior_alternative4_evd(
#     ich_aggressive
#   ),
#   m_posterior_alternative1_mrs_90 = f_posterior_alternative1_mrs_90(
#     ich_aggressive
#   ),
#   m_posterior_alternative2_mrs_90 = f_posterior_alternative2_mrs_90(
#     ich_aggressive
#   ),
#   m_posterior_alternative3_mrs_90 = f_posterior_alternative3_mrs_90(
#     ich_aggressive
#   ),
#   m_posterior_alternative4_mrs_90 = f_posterior_alternative4_mrs_90(
#     ich_aggressive
#   ),
#   m_posterior_alternative5_mrs_90 = f_posterior_alternative5_mrs_90(
#     ich_aggressive
#   ),

#   ## Output and Reports ----
#   table1 = table_1_function(ich_aggressive),
#   table1_docx = gtsave(table1, here("manuscripts", "table1.docx")),
#   neutral_aggressive_models = models_list_func(
#     m_posterior_neutral_neurosurgery,
#     m_posterior_neutral_evd,
#     m_posterior_neutral_days_mechanical_ventilation,
#     m_posterior_neutral_tracheostomy,
#     m_posterior_neutral_comfort,
#     m_posterior_neutral_early_wlst,
#     m_posterior_neutral_dnr_binary
#   ),
#   left_aggressive_models = models_list_func(
#     m_posterior_left_neurosurgery,
#     m_posterior_left_evd,
#     m_posterior_left_days_mechanical_ventilation,
#     m_posterior_left_tracheostomy,
#     m_posterior_left_comfort,
#     m_posterior_left_early_wlst,
#     m_posterior_left_dnr_binary
#   ),
#   right_aggressive_models = models_list_func(
#     m_posterior_right_neurosurgery,
#     m_posterior_right_evd,
#     m_posterior_right_days_mechanical_ventilation,
#     m_posterior_right_tracheostomy,
#     m_posterior_right_comfort,
#     m_posterior_right_early_wlst,
#     m_posterior_right_dnr_binary
#   ),
#   flat_aggressive_models = models_list_func(
#     m_posterior_flat_neurosurgery,
#     m_posterior_flat_evd,
#     m_posterior_flat_days_mechanical_ventilation,
#     m_posterior_flat_tracheostomy,
#     m_posterior_flat_comfort,
#     m_posterior_flat_early_wlst,
#     m_posterior_flat_dnr_binary
#   ),
#   table2_neutral = table_2_function(ich_aggressive, neutral_aggressive_models),
#   table2_left = table_2_function(ich_aggressive, left_aggressive_models),
#   table2_right = table_2_function(ich_aggressive, right_aggressive_models),
#   table2_flat = table_2_function(ich_aggressive, flat_aggressive_models),
#   table2_docx = gtsave(table2_neutral, here("manuscripts", "table2.docx")),
#   figure_1 = posterior_odds_plot(
#     models = list(
#       "Neutral Prior" = m_posterior_neutral_neurosurgery,
#       "Left Prior" = m_posterior_left_neurosurgery,
#       "Right Prior" = m_posterior_right_neurosurgery,
#       "Flat Prior" = m_posterior_flat_neurosurgery
#     )
#   ),
#   figure_1_tiff = ggsave(
#     here("manuscripts", "figure1.tiff"),
#     plot = figure_1,
#     height = 10,
#     width = 8,
#     dpi = 1200,
#     scale = 1
#   ),
#   figure_1_svg = ggsave(
#     here("manuscripts", "figure1.svg"),
#     plot = figure_1,
#     height = 10,
#     width = 8,
#     dpi = 1200,
#     scale = 1
#   ),
#   figure_1_png = ggsave(
#     here("manuscripts", "figure1.png"),
#     plot = figure_1,
#     height = 10,
#     width = 9,
#     dpi = 1200,
#     scale = 1
#   ),
#   neutral_outcome_90_models = outcome_models_list_func(
#     m_posterior_neutral_mrs_90_canonical,
#     m_posterior_neutral_euro_mobility_90_canonical,
#     m_posterior_neutral_euro_selfcare_90_canonical,
#     m_posterior_neutral_euro_usual_90_canonical,
#     m_posterior_neutral_euro_pain_90_canonical,
#     m_posterior_neutral_euro_anxiety_90_canonical,
#     m_vas_90
#   ),
#   neutral_outcome_180_models = outcome_models_list_func(
#     m_posterior_neutral_mrs_180_canonical,
#     m_posterior_neutral_euro_mobility_180_canonical,
#     m_posterior_neutral_euro_selfcare_180_canonical,
#     m_posterior_neutral_euro_usual_180_canonical,
#     m_posterior_neutral_euro_pain_180_canonical,
#     m_posterior_neutral_euro_anxiety_180_canonical,
#     m_vas_180
#   ),
#   neutral_outcome_365_models = outcome_models_list_func(
#     m_posterior_neutral_mrs_365_canonical,
#     m_posterior_neutral_euro_mobility_365_canonical,
#     m_posterior_neutral_euro_selfcare_365_canonical,
#     m_posterior_neutral_euro_usual_365_canonical,
#     m_posterior_neutral_euro_pain_365_canonical,
#     m_posterior_neutral_euro_anxiety_365_canonical,
#     m_vas_365
#   ),
#   table4_neutral = table_3_updated(neutral_outcome_90_models),
#   table4_180 = table_3_function(neutral_outcome_180_models),
#   table4_365 = table_3_function(neutral_outcome_365_models),
#   table4_docx = gtsave(table4_neutral, here("manuscripts", "table4.docx")),
#   left_outcome_90_models = outcome_models_list_func(
#     m_posterior_left_mrs_90_canonical,
#     m_posterior_left_euro_mobility_90_canonical,
#     m_posterior_left_euro_selfcare_90_canonical,
#     m_posterior_left_euro_usual_90_canonical,
#     m_posterior_left_euro_pain_90_canonical,
#     m_posterior_left_euro_anxiety_90_canonical,
#     m_vas_90
#   ),
#   left_outcome_180_models = outcome_models_list_func(
#     m_posterior_left_mrs_180_canonical,
#     m_posterior_left_euro_mobility_180_canonical,
#     m_posterior_left_euro_selfcare_180_canonical,
#     m_posterior_left_euro_usual_180_canonical,
#     m_posterior_left_euro_pain_180_canonical,
#     m_posterior_left_euro_anxiety_180_canonical,
#     m_vas_180
#   ),
#   left_outcome_365_models = outcome_models_list_func(
#     m_posterior_left_mrs_365_canonical,
#     m_posterior_left_euro_mobility_365_canonical,
#     m_posterior_left_euro_selfcare_365_canonical,
#     m_posterior_left_euro_usual_365_canonical,
#     m_posterior_left_euro_pain_365_canonical,
#     m_posterior_left_euro_anxiety_365_canonical,
#     m_vas_365
#   ),
#   table4_left = table_3_function(left_outcome_90_models),
#   right_outcome_90_models = outcome_models_list_func(
#     m_posterior_right_mrs_90_canonical,
#     m_posterior_right_euro_mobility_90_canonical,
#     m_posterior_right_euro_selfcare_90_canonical,
#     m_posterior_right_euro_usual_90_canonical,
#     m_posterior_right_euro_pain_90_canonical,
#     m_posterior_right_euro_anxiety_90_canonical,
#     m_vas_90
#   ),
#   right_outcome_180_models = outcome_models_list_func(
#     m_posterior_right_mrs_180_canonical,
#     m_posterior_right_euro_mobility_180_canonical,
#     m_posterior_right_euro_selfcare_180_canonical,
#     m_posterior_right_euro_usual_180_canonical,
#     m_posterior_right_euro_pain_180_canonical,
#     m_posterior_right_euro_anxiety_180_canonical,
#     m_vas_180
#   ),
#   right_outcome_365_models = outcome_models_list_func(
#     m_posterior_right_mrs_365_canonical,
#     m_posterior_right_euro_mobility_365_canonical,
#     m_posterior_right_euro_selfcare_365_canonical,
#     m_posterior_right_euro_usual_365_canonical,
#     m_posterior_right_euro_pain_365_canonical,
#     m_posterior_right_euro_anxiety_365_canonical,
#     m_vas_365
#   ),
#   table4_right = table_3_function(right_outcome_90_models),
#   flat_outcome_90_models = outcome_models_list_func(
#     m_posterior_flat_mrs_90_canonical,
#     m_posterior_flat_euro_mobility_90_canonical,
#     m_posterior_flat_euro_selfcare_90_canonical,
#     m_posterior_flat_euro_usual_90_canonical,
#     m_posterior_flat_euro_pain_90_canonical,
#     m_posterior_flat_euro_anxiety_90_canonical,
#     m_vas_90
#   ),
#   flat_outcome_180_models = outcome_models_list_func(
#     m_posterior_flat_mrs_180_canonical,
#     m_posterior_flat_euro_mobility_180_canonical,
#     m_posterior_flat_euro_selfcare_180_canonical,
#     m_posterior_flat_euro_usual_180_canonical,
#     m_posterior_flat_euro_pain_180_canonical,
#     m_posterior_flat_euro_anxiety_180_canonical,
#     m_vas_180
#   ),
#   flat_outcome_365_models = outcome_models_list_func(
#     m_posterior_flat_mrs_365_canonical,
#     m_posterior_flat_euro_mobility_365_canonical,
#     m_posterior_flat_euro_selfcare_365_canonical,
#     m_posterior_flat_euro_usual_365_canonical,
#     m_posterior_flat_euro_pain_365_canonical,
#     m_posterior_flat_euro_anxiety_365_canonical,
#     m_vas_365
#   ),
#   table4_flat = table_3_function(flat_outcome_90_models),
#   mrs_90_fig = mrs_figure_function(ich_aggressive, mrs_90),
#   mrs_90_png = ggsave(
#     here("manuscripts", "mrs90.png"),
#     plot = mrs_90_fig,
#     height = 14,
#     width = 25,
#     dpi = 600,
#     bg = "white",
#   ),
#   mrs_180_fig = mrs_figure_function(ich_aggressive, mrs_180),
#   mrs_365_fig = mrs_figure_function(ich_aggressive, mrs_365),
#   euro_90_mobility_fig = euro_figure_function(ich_aggressive, euro_mobility_90),
#   euro_mobility_90_png = ggsave(
#     here("manuscripts", "euro_90_mobility.png"),
#     plot = euro_90_mobility_fig,
#     height = 14,
#     width = 20,
#     dpi = 600,
#     bg = "white",
#   ),
#   euro_90_selfcare_fig = euro_figure_function(ich_aggressive, euro_selfcare_90),
#   euro_selfcare_90_png = ggsave(
#     here("manuscripts", "euro_90_selfcare.png"),
#     plot = euro_90_selfcare_fig,
#     height = 14,
#     width = 20,
#     dpi = 600,
#     bg = "white",
#   ),
#   euro_90_usual_fig = euro_figure_function(ich_aggressive, euro_usual_90),
#   euro_usual_90_png = ggsave(
#     here("manuscripts", "euro_90_usual.png"),
#     plot = euro_90_usual_fig,
#     height = 14,
#     width = 20,
#     dpi = 600,
#     bg = "white",
#   ),
#   euro_90_pain_fig = euro_figure_function(ich_aggressive, euro_pain_90),
#   euro_pain_90_png = ggsave(
#     here("manuscripts", "euro_90_pain.png"),
#     plot = euro_90_pain_fig,
#     height = 14,
#     width = 20,
#     dpi = 600,
#     bg = "white",
#   ),
#   euro_90_anxiety_fig = euro_figure_function(ich_aggressive, euro_anxiety_90),
#   euro_anxiety_90_png = ggsave(
#     here("manuscripts", "euro_90_anxiety.png"),
#     plot = euro_90_anxiety_fig,
#     height = 14,
#     width = 20,
#     dpi = 600,
#     bg = "white",
#   ),
#   euro_180_mobility_fig = euro_figure_function(
#     ich_aggressive,
#     euro_mobility_180
#   ),
#   euro_180_selfcare_fig = euro_figure_function(
#     ich_aggressive,
#     euro_selfcare_180
#   ),
#   euro_180_usual_fig = euro_figure_function(ich_aggressive, euro_usual_180),
#   euro_180_pain_fig = euro_figure_function(ich_aggressive, euro_pain_180),
#   euro_180_anxiety_fig = euro_figure_function(ich_aggressive, euro_anxiety_180),
#   euro_365_mobility_fig = euro_figure_function(
#     ich_aggressive,
#     euro_mobility_365
#   ),
#   euro_365_selfcare_fig = euro_figure_function(
#     ich_aggressive,
#     euro_selfcare_365
#   ),
#   euro_365_usual_fig = euro_figure_function(ich_aggressive, euro_usual_365),
#   euro_365_pain_fig = euro_figure_function(ich_aggressive, euro_pain_365),
#   euro_365_anxiety_fig = euro_figure_function(ich_aggressive, euro_anxiety_365),
#   euro_vas_90_plot = vas_plot_function(ich_aggressive, euro_vas_90),
#   euro_vas_90_png = ggsave(
#     here("manuscripts", "euro_90_vas.png"),
#     plot = euro_vas_90_plot,
#     height = 14,
#     width = 20,
#     dpi = 600,
#     bg = "white",
#   ),
#   euro_vas_180_plot = vas_plot_function(ich_aggressive, euro_vas_180),
#   euro_vas_365_plot = vas_plot_function(ich_aggressive, euro_vas_365),

#   neurosurgery_imp = f_posterior_neutral_neurosurgery_imp(ich_imputed),
#   evd_imp = f_posterior_neutral_evd_imp(ich_imputed),
#   mrs_imp = f_posterior_neutral_mrs_90_canonical_imp(ich_imputed),
#   table_imp = f_imp_sensitivity(neurosurgery_imp, evd_imp, mrs_imp),
#   alternative_table_neurosurgery = f_alternative_adjustment_table(
#     m_posterior_minimal_neurosurgery,
#     m_posterior_alternative1_neurosurgery,
#     m_posterior_alternative2_neurosurgery,
#     m_posterior_alternative3_neurosurgery,
#     m_posterior_alternative4_neurosurgery
#   ),
#   alternative_table_evd = f_alternative_adjustment_table(
#     m_posterior_minimal_evd,
#     m_posterior_alternative1_evd,
#     m_posterior_alternative2_evd,
#     m_posterior_alternative3_evd,
#     m_posterior_alternative4_evd
#   ),
#   alternative_table_mrs90 = f_alternative_adjustment_table_mrs(
#     m_posterior_mrs_90_minimal,
#     m_posterior_alternative1_mrs_90,
#     m_posterior_alternative2_mrs_90,
#     m_posterior_alternative3_mrs_90,
#     m_posterior_alternative4_mrs_90,
#     m_posterior_alternative5_mrs_90
#   )
# )
