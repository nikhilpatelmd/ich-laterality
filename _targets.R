library(targets)
library(tarchetypes)

# General pipeline settings ----

suppressPackageStartupMessages(library(brms))

# Taken from Andrew Heiss (https://github.com/andrewheiss/cautioning-canary/blob/master/_targets.R)

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

  ## Exploratory data analysis ----
  table_1_aggressive = table_1_function(ich_aggressive),
  table_2_aggressive = table_2_aggressive_function(ich_aggressive),
  table_1_outcomes = table_1_function(ich_all),

  ## DAGs ----
  dag_aggressive = aggressive_dag_function(x),
  dag_outcomes = outcomes_dag_function(x),

  ## Priors ----
  settings = model_setup(),
  m_prior_neurosurgery_minimal = f_prior_neurosurgery_minimal(ich_aggressive),
  m_prior_neurosurgery_canonical = f_prior_neurosurgery_canonical(ich_aggressive),
  m_prior_evd_minimal = f_prior_evd_minimal(ich_aggressive),
  m_prior_evd_canonical = f_prior_evd_canonical(ich_aggressive),
  m_prior_trach_minimal = f_prior_trach_minimal(ich_aggressive),
  m_prior_trach_canonical = f_prior_trach_canonical(ich_aggressive),
  m_prior_vent_minimal = f_prior_vent_minimal(ich_aggressive),
  m_prior_vent_canonical = f_prior_vent_canonical(ich_aggressive),
  m_prior_comfort_minimal = f_prior_comfort_minimal(ich_aggressive),
  m_prior_comfort_canonical = f_prior_comfort_canonical(ich_aggressive),
  m_prior_early_comfort_minimal = f_prior_early_comfort_minimal(ich_aggressive),
  m_prior_early_comfort_canonical = f_prior_early_comfort_canonical(ich_aggressive),
  m_prior_dnr_minimal = f_prior_dnr_minimal(ich_aggressive),
  m_prior_dnr_canonical = f_prior_dnr_canonical(ich_aggressive),

  ## Posterior Simulation ----
  m_posterior_neurosurgery_minimal = f_posterior_neurosurgery_minimal(ich_aggressive),
  m_posterior_neurosurgery_canonical = f_posterior_neurosurgery_canonical(ich_aggressive),
  m_posterior_evd_minimal = f_posterior_evd_minimal(ich_aggressive),
  m_posterior_evd_canonical = f_posterior_evd_canonical(ich_aggressive),
  m_posterior_trach_minimal = f_posterior_trach_minimal(ich_aggressive),
  m_posterior_trach_canonical = f_posterior_trach_canonical(ich_aggressive),
  m_posterior_vent_minimal = f_posterior_vent_minimal(ich_aggressive),
  m_posterior_vent_canonical = f_posterior_vent_canonical(ich_aggressive),
  m_posterior_comfort_minimal = f_posterior_comfort_minimal(ich_aggressive),
  m_posterior_comfort_canonical = f_posterior_comfort_canonical(ich_aggressive),
  m_posterior_early_comfort_minimal = f_posterior_early_comfort_minimal(ich_aggressive),
  m_posterior_early_comfort_canonical = f_posterior_early_comfort_canonical(ich_aggressive),
  m_posterior_dnr_minimal = f_posterior_dnr_minimal(ich_aggressive),
  m_posterior_dnr_canonical = f_posterior_dnr_canonical(ich_aggressive),

  ## Diagnostics ----
  neurosugery_minimal_diagnostics = posterior_diagnostics(m_posterior_neurosurgery_minimal),
  neurosugery_canonical_diagnostics = posterior_diagnostics(m_posterior_neurosurgery_canonical),
  evd_minimal_diagnostics = posterior_diagnostics(m_posterior_evd_minimal),
  evd_canonical_diagnostics = posterior_diagnostics(m_posterior_evd_canonical),
  trach_minimal_diagnostics = posterior_diagnostics(m_posterior_trach_minimal),
  trach_canonical_diagnostics = posterior_diagnostics(m_posterior_trach_canonical),
  vent_minimal_diagnostics = posterior_diagnostics(m_posterior_vent_minimal),
  vent_canonical_diagnostics = posterior_diagnostics(m_posterior_vent_canonical),
  comfort_care_minimal_diagnostics = posterior_diagnostics(m_posterior_comfort_minimal),
  comfort_care_canonical_diagnostics = posterior_diagnostics(m_posterior_comfort_canonical),
  early_wlst_minimal_diagnostics = posterior_diagnostics(m_posterior_early_comfort_minimal),
  early_wlst_canonical_diagnostics = posterior_diagnostics(m_posterior_early_comfort_canonical),
  dnr_minimal_diagnostics = posterior_diagnostics(m_posterior_dnr_minimal),
  dnr_canonical_diagnostics = posterior_diagnostics(m_posterior_dnr_canonical),

  ## Plots ----
  neurosurgery_minimal_visuals = results_visual(m_posterior_neurosurgery_minimal, "Neurosurgical Intervention"),
  neurosurgery_canonical_visuals = results_visual(m_posterior_neurosurgery_canonical, "Neurosurgical Intervention"),
  evd_minimal_visuals = results_visual(m_posterior_evd_minimal, "External Ventricular Drain"),
  evd_canonical_visuals = results_visual(m_posterior_evd_canonical, "External Ventricular Drain"),
  trach_minimal_visuals = results_visual(m_posterior_trach_minimal, "Tracheostomy"),
  trach_canonical_visuals = results_visual(m_posterior_trach_canonical, "Tracheostomy"),
  vent_minimal_visuals = results_visual(m_posterior_vent_minimal, "Days of Mechanical Ventilation"),
  vent_canonical_visuals = results_visual(m_posterior_vent_canonical, "Days of Mechanical Ventilation"),
  comfort_minimal_visuals = results_visual(m_posterior_comfort_minimal, "Comfort Care"),
  comfort_canonical_visuals = results_visual(m_posterior_comfort_canonical, "Comfort Care"),
  early_wlst_minimal_visuals = results_visual(m_posterior_early_comfort_minimal, "Early Comfort Care"),
  early_wlst_canonical_visuals = results_visual(m_posterior_early_comfort_canonical, "Early Comfort Care"),
  dnr_minimal_visuals = results_visual(m_posterior_dnr_minimal, "DNR Order"),
  dnr_canonical_visuals = results_visual(m_posterior_dnr_canonical, "DNR Order"),

  ## Reports ----
  # tar_quarto(
  #   aggressive,
  #   "reports/aggressiveness.qmd"
  # ),
  # tar_quarto(
  #   outcomes,
  #   "reports/outcomes.qmd"
  # ),
  # tar_quarto(
  #   prior_report,
  #   "reports/priors.qmd"
  # ),
  # tar_quarto(
  #   posterior_report,
  #   "reports/posteriors.qmd"
  # )
)
