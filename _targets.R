library(targets)
library(tarchetypes)
library(quarto)


# General pipeline settings ----

suppressPackageStartupMessages(library(brms))

# Taken from Andrew Heiss (https://github.com/andrewheiss/cautioning-canary/blob/master/_targets.R)

#   - If using cmdstanr, you can use within-chain threading
#   - (https://cran.r-project.org/web/packages/brms/vignettes/brms_threading.html)
#   - On a four-core computer, use mc.cores = 2 and threads = threading(2)
#   - On an eight+-core computer, use mc.cores = 4 and threads = threading(2)
options(
  mc.cores = 3,
  mc.threads = 2,
  brms.backend = "cmdstanr"
)

options(
  tidyverse.quiet = TRUE,
  dplyr.summarise.inform = FALSE
)

# R functions ----

source("R/packages.R")
source("R/data_cleaning.R")
source("R/exploratory_data_analysis.R")
source("R/dags.R")
source("R/aggressive_care_prior_models.R")
source("R/aggressive_care_posterior_models.R")
source("R/diagnostics.R")
source("R/results_visualization.R")
source("R/outcomes_prior_models.R")
source("R/outcomes_posterior_models.R")

# Pipeline ----
tar_plan(

  ## Define raw data file ----
  tar_file_read(
    imported_data,
    "data/raw_data/all.rds",
    read_rds(!!.x)
  ),

  ## Select, filter, and clean data ----
  selected_data = select_variables(imported_data),
  ich_all = filter_variables(selected_data),
  ich_aggressive = ich_all |> filter(study == "ERICH" | study == "ATACH-2") |> droplevels(),
  erich = ich_all |> filter(study == "ERICH") |> droplevels(),
  atach = ich_all |> filter(study == "ATACH-2") |> droplevels(),

  ## Exploratory data analysis ----
  table_1_aggressive = table_1_function(ich_aggressive),
  table_2_aggressive = table_2_aggressive_function(ich_aggressive),
  table_1_outcomes = table_1_function(ich_all),

  ## DAGs ----
  dag_aggressive = aggressive_dag_function(x),
  dag_outcomes = outcomes_dag_function(x),

  ## Aggressive Care ----

  ### Priors ----
  settings = model_setup(),
  m_prior_neutral_neurosurgery = f_prior_neutral_neurosurgery(ich_aggressive),
  m_prior_left_neurosurgery = f_prior_left_neurosurgery(ich_aggressive),
  m_prior_right_neurosurgery = f_prior_right_neurosurgery(ich_aggressive),
  m_prior_flat_neurosurgery = f_prior_flat_neurosurgery(ich_aggressive),
  m_prior_neutral_evd = f_prior_neutral_evd(ich_aggressive),
  m_prior_left_evd = f_prior_left_evd(ich_aggressive),
  m_prior_right_evd = f_prior_right_evd(ich_aggressive),
  m_prior_flat_evd = f_prior_flat_evd(ich_aggressive),
  m_prior_neutral_tracheostomy = f_prior_neutral_tracheostomy(ich_aggressive),
  m_prior_left_tracheostomy = f_prior_left_tracheostomy(ich_aggressive),
  m_prior_right_tracheostomy = f_prior_right_tracheostomy(ich_aggressive),
  m_prior_flat_tracheostomy = f_prior_flat_tracheostomy(ich_aggressive),
  m_prior_neutral_days_mechanical_ventilation = f_prior_neutral_days_mechanical_ventilation(ich_aggressive),
  m_prior_left_days_mechanical_ventilation = f_prior_left_days_mechanical_ventilation(ich_aggressive),
  m_prior_right_days_mechanical_ventilation = f_prior_right_days_mechanical_ventilation(ich_aggressive),
  m_prior_flat_days_mechanical_ventilation = f_prior_flat_days_mechanical_ventilation(ich_aggressive),
  m_prior_neutral_comfort = f_prior_neutral_comfort(ich_aggressive),
  m_prior_left_comfort = f_prior_left_comfort(ich_aggressive),
  m_prior_right_comfort = f_prior_right_comfort(ich_aggressive),
  m_prior_flat_comfort = f_prior_flat_comfort(ich_aggressive),
  m_prior_neutral_early_wlst = f_prior_neutral_early_wlst(ich_aggressive),
  m_prior_left_early_wlst = f_prior_left_early_wlst(ich_aggressive),
  m_prior_right_early_wlst = f_prior_right_early_wlst(ich_aggressive),
  m_prior_flat_early_wlst = f_prior_flat_early_wlst(ich_aggressive),
  m_prior_neutral_dnr_binary = f_prior_neutral_dnr_binary(ich_aggressive),
  m_prior_left_dnr_binary = f_prior_left_dnr_binary(ich_aggressive),
  m_prior_right_dnr_binary = f_prior_right_dnr_binary(ich_aggressive),
  m_prior_flat_dnr_binary = f_prior_flat_dnr_binary(ich_aggressive),

  ### Posterior Simulation ----
  m_posterior_neutral_neurosurgery = f_posterior_neutral_neurosurgery(ich_aggressive),
  m_posterior_left_neurosurgery = f_posterior_left_neurosurgery(ich_aggressive),
  m_posterior_right_neurosurgery = f_posterior_right_neurosurgery(ich_aggressive),
  m_posterior_flat_neurosurgery = f_posterior_flat_neurosurgery(ich_aggressive),
  m_posterior_neutral_evd = f_posterior_neutral_evd(ich_aggressive),
  m_posterior_left_evd = f_posterior_left_evd(ich_aggressive),
  m_posterior_right_evd = f_posterior_right_evd(ich_aggressive),
  m_posterior_flat_evd = f_posterior_flat_evd(ich_aggressive),
  m_posterior_neutral_tracheostomy = f_posterior_neutral_tracheostomy(ich_aggressive),
  m_posterior_left_tracheostomy = f_posterior_left_tracheostomy(ich_aggressive),
  m_posterior_right_tracheostomy = f_posterior_right_tracheostomy(ich_aggressive),
  m_posterior_flat_tracheostomy = f_posterior_flat_tracheostomy(ich_aggressive),
  m_posterior_neutral_days_mechanical_ventilation = f_posterior_neutral_days_mechanical_ventilation(ich_aggressive),
  m_posterior_left_days_mechanical_ventilation = f_posterior_left_days_mechanical_ventilation(ich_aggressive),
  m_posterior_right_days_mechanical_ventilation = f_posterior_right_days_mechanical_ventilation(ich_aggressive),
  m_posterior_flat_days_mechanical_ventilation = f_posterior_flat_days_mechanical_ventilation(ich_aggressive),
  m_posterior_neutral_comfort = f_posterior_neutral_comfort(ich_aggressive),
  m_posterior_left_comfort = f_posterior_left_comfort(ich_aggressive),
  m_posterior_right_comfort = f_posterior_right_comfort(ich_aggressive),
  m_posterior_flat_comfort = f_posterior_flat_comfort(ich_aggressive),
  m_posterior_neutral_early_wlst = f_posterior_neutral_early_wlst(ich_aggressive),
  m_posterior_left_early_wlst = f_posterior_left_early_wlst(ich_aggressive),
  m_posterior_right_early_wlst = f_posterior_right_early_wlst(ich_aggressive),
  m_posterior_flat_early_wlst = f_posterior_flat_early_wlst(ich_aggressive),
  m_posterior_neutral_dnr_binary = f_posterior_neutral_dnr_binary(ich_aggressive),
  m_posterior_left_dnr_binary = f_posterior_left_dnr_binary(ich_aggressive),
  m_posterior_right_dnr_binary = f_posterior_right_dnr_binary(ich_aggressive),
  m_posterior_flat_dnr_binary = f_posterior_flat_dnr_binary(ich_aggressive),

  ## Outcomes ----

  ### Priors ----
  # m_prior_mrs_90_minimal = f_prior_mrs_90_minimal(ich_all),
  # m_prior_mrs_90_canonical = f_prior_mrs_90_canonical(ich_all),
  # m_prior_mrs_180_minimal = f_prior_mrs_180_minimal(ich_all),
  # m_prior_mrs_180_canonical = f_prior_mrs_180_canonical(ich_all),
  # m_prior_mrs_365_minimal = f_prior_mrs_365_minimal(ich_all),
  # m_prior_mrs_365_canonical = f_prior_mrs_365_canonical(ich_all),
  # m_prior_euro_mobility_90_minimal = f_prior_euro_mobility_90_minimal(ich_all),
  # m_prior_euro_mobility_90_canonical = f_prior_euro_mobility_90_canonical(ich_all),
  # m_prior_euro_mobility_180_minimal = f_prior_euro_mobility_180_minimal(ich_all),
  # m_prior_euro_mobility_180_canonical = f_prior_euro_mobility_180_canonical(ich_all),
  # m_prior_euro_mobility_365_minimal = f_prior_euro_mobility_365_minimal(ich_all),
  # m_prior_euro_mobility_365_canonical = f_prior_euro_mobility_365_canonical(ich_all),
  # m_prior_euro_selfcare_90_minimal = f_prior_euro_selfcare_90_minimal(ich_all),
  # m_prior_euro_selfcare_90_canonical = f_prior_euro_selfcare_90_canonical(ich_all),
  # m_prior_euro_selfcare_180_minimal = f_prior_euro_selfcare_180_minimal(ich_all),
  # m_prior_euro_selfcare_180_canonical = f_prior_euro_selfcare_180_canonical(ich_all),
  # m_prior_euro_selfcare_365_minimal = f_prior_euro_selfcare_365_minimal(ich_all),
  # m_prior_euro_selfcare_365_canonical = f_prior_euro_selfcare_365_canonical(ich_all),
  # m_prior_euro_usual_90_minimal = f_prior_euro_usual_90_minimal(ich_all),
  # m_prior_euro_usual_90_canonical = f_prior_euro_usual_90_canonical(ich_all),
  # m_prior_euro_usual_180_minimal = f_prior_euro_usual_180_minimal(ich_all),
  # m_prior_euro_usual_180_canonical = f_prior_euro_usual_180_canonical(ich_all),
  # m_prior_euro_usual_365_minimal = f_prior_euro_usual_365_minimal(ich_all),
  # m_prior_euro_usual_365_canonical = f_prior_euro_usual_365_canonical(ich_all),
  # m_prior_euro_pain_90_minimal = f_prior_euro_pain_90_minimal(ich_all),
  # m_prior_euro_pain_90_canonical = f_prior_euro_pain_90_canonical(ich_all),
  # m_prior_euro_pain_180_minimal = f_prior_euro_pain_180_minimal(ich_all),
  # m_prior_euro_pain_180_canonical = f_prior_euro_pain_180_canonical(ich_all),
  # m_prior_euro_pain_365_minimal = f_prior_euro_pain_365_minimal(ich_all),
  # m_prior_euro_pain_365_canonical = f_prior_euro_pain_365_canonical(ich_all),
  # m_prior_euro_anxiety_90_minimal = f_prior_euro_anxiety_90_minimal(ich_all),
  # m_prior_euro_anxiety_90_canonical = f_prior_euro_anxiety_90_canonical(ich_all),
  # m_prior_euro_anxiety_180_minimal = f_prior_euro_anxiety_180_minimal(ich_all),
  # m_prior_euro_anxiety_180_canonical = f_prior_euro_anxiety_180_canonical(ich_all),
  # m_prior_euro_anxiety_365_minimal = f_prior_euro_anxiety_365_minimal(ich_all),
  # m_prior_euro_anxiety_365_canonical = f_prior_euro_anxiety_365_canonical(ich_all),


  # ### Posterior Simulation ----
  # m_posterior_mrs_90_minimal = f_posterior_mrs_90_minimal(ich_all),
  # m_posterior_mrs_90_canonical = f_posterior_mrs_90_canonical(ich_all),
  # m_posterior_mrs_180_minimal = f_posterior_mrs_180_minimal(ich_all),
  # m_posterior_mrs_180_canonical = f_posterior_mrs_180_canonical(ich_all),
  # m_posterior_mrs_365_minimal = f_posterior_mrs_365_minimal(ich_all),
  # m_posterior_mrs_365_canonical = f_posterior_mrs_365_canonical(ich_all),
  # m_posterior_euro_mobility_90_minimal = f_posterior_euro_mobility_90_minimal(ich_all),
  # m_posterior_euro_mobility_90_canonical = f_posterior_euro_mobility_90_canonical(ich_all),
  # m_posterior_euro_mobility_180_minimal = f_posterior_euro_mobility_180_minimal(ich_all),
  # m_posterior_euro_mobility_180_canonical = f_posterior_euro_mobility_180_canonical(ich_all),
  # m_posterior_euro_mobility_365_minimal = f_posterior_euro_mobility_365_minimal(ich_all),
  # m_posterior_euro_mobility_365_canonical = f_posterior_euro_mobility_365_canonical(ich_all),
  # m_posterior_euro_selfcare_90_minimal = f_posterior_euro_selfcare_90_minimal(ich_all),
  # m_posterior_euro_selfcare_90_canonical = f_posterior_euro_selfcare_90_canonical(ich_all),
  # m_posterior_euro_selfcare_180_minimal = f_posterior_euro_selfcare_180_minimal(ich_all),
  # m_posterior_euro_selfcare_180_canonical = f_posterior_euro_selfcare_180_canonical(ich_all),
  # m_posterior_euro_selfcare_365_minimal = f_posterior_euro_selfcare_365_minimal(ich_all),
  # m_posterior_euro_selfcare_365_canonical = f_posterior_euro_selfcare_365_canonical(ich_all),
  # m_posterior_euro_usual_90_minimal = f_posterior_euro_usual_90_minimal(ich_all),
  # m_posterior_euro_usual_90_canonical = f_posterior_euro_usual_90_canonical(ich_all),
  # m_posterior_euro_usual_180_minimal = f_posterior_euro_usual_180_minimal(ich_all),
  # m_posterior_euro_usual_180_canonical = f_posterior_euro_usual_180_canonical(ich_all),
  # m_posterior_euro_usual_365_minimal = f_posterior_euro_usual_365_minimal(ich_all),
  # m_posterior_euro_usual_365_canonical = f_posterior_euro_usual_365_canonical(ich_all),
  # m_posterior_euro_pain_90_minimal = f_posterior_euro_pain_90_minimal(ich_all),
  # m_posterior_euro_pain_90_canonical = f_posterior_euro_pain_90_canonical(ich_all),
  # m_posterior_euro_pain_180_minimal = f_posterior_euro_pain_180_minimal(ich_all),
  # m_posterior_euro_pain_180_canonical = f_posterior_euro_pain_180_canonical(ich_all),
  # m_posterior_euro_pain_365_minimal = f_posterior_euro_pain_365_minimal(ich_all),
  # m_posterior_euro_pain_365_canonical = f_posterior_euro_pain_365_canonical(ich_all),
  # m_posterior_euro_anxiety_90_minimal = f_posterior_euro_anxiety_90_minimal(ich_all),
  # m_posterior_euro_anxiety_90_canonical = f_posterior_euro_anxiety_90_canonical(ich_all),
  # m_posterior_euro_anxiety_180_minimal = f_posterior_euro_anxiety_180_minimal(ich_all),
  # m_posterior_euro_anxiety_180_canonical = f_posterior_euro_anxiety_180_canonical(ich_all),
  # m_posterior_euro_anxiety_365_minimal = f_posterior_euro_anxiety_365_minimal(ich_all),
  # m_posterior_euro_anxiety_365_canonical = f_posterior_euro_anxiety_365_canonical(ich_all),

  # ## Reports ----
  # tar_quarto(
  #   aggressive_manuscript,
  #   "manuscripts/manuscript1.qmd"
  # )
)
