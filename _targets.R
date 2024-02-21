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

  ### Posterior Simulation ----
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

  #### Subgroups with ERICH ----
  m_posterior_neurosurgery_minimal_erich = f_posterior_neurosurgery_minimal(erich),
  m_posterior_neurosurgery_canonical_erich = f_posterior_neurosurgery_canonical(erich),
  m_posterior_evd_minimal_erich = f_posterior_evd_minimal(erich),
  m_posterior_evd_canonical_erich = f_posterior_evd_canonical(erich),
  m_posterior_trach_minimal_erich = f_posterior_trach_minimal(erich),
  m_posterior_trach_canonical_erich = f_posterior_trach_canonical(erich),
  m_posterior_comfort_minimal_erich = f_posterior_comfort_minimal(erich),
  m_posterior_comfort_canonical_erich = f_posterior_comfort_canonical(erich),
  m_posterior_early_comfort_minimal_erich = f_posterior_early_comfort_minimal(erich),
  m_posterior_early_comfort_canonical_erich = f_posterior_early_comfort_canonical(erich),
  m_posterior_dnr_minimal_erich = f_posterior_dnr_minimal(erich),
  m_posterior_dnr_canonical_erich = f_posterior_dnr_canonical(erich),

  #### Subgroups with ATACH ----
  m_posterior_neurosurgery_minimal_atach = f_posterior_neurosurgery_minimal(atach),
  m_posterior_neurosurgery_canonical_atach = f_posterior_neurosurgery_canonical(atach),
  m_posterior_evd_minimal_atach = f_posterior_evd_minimal(atach),
  m_posterior_evd_canonical_atach = f_posterior_evd_canonical(atach),
  m_posterior_trach_minimal_atach = f_posterior_trach_minimal(atach),
  m_posterior_trach_canonical_atach = f_posterior_trach_canonical(atach),
  m_posterior_comfort_minimal_atach = f_posterior_comfort_minimal(atach),
  m_posterior_comfort_canonical_atach = f_posterior_comfort_canonical(atach),
  m_posterior_early_comfort_minimal_atach = f_posterior_early_comfort_minimal(atach),
  m_posterior_early_comfort_canonical_atach = f_posterior_early_comfort_canonical(atach),
  m_posterior_dnr_minimal_atach = f_posterior_dnr_minimal(atach),
  m_posterior_dnr_canonical_atach = f_posterior_dnr_canonical(atach),

  ### Diagnostics ----
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

  ### Plots ----
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

  ## Outcomes ----

  ### Priors ----
  m_prior_mrs_90_minimal = f_prior_mrs_90_minimal(ich_all),
  m_prior_mrs_90_canonical = f_prior_mrs_90_canonical(ich_all),
  m_prior_mrs_180_minimal = f_prior_mrs_180_minimal(ich_all),
  m_prior_mrs_180_canonical = f_prior_mrs_180_canonical(ich_all),
  m_prior_mrs_365_minimal = f_prior_mrs_365_minimal(ich_all),
  m_prior_mrs_365_canonical = f_prior_mrs_365_canonical(ich_all),
  m_prior_euro_mobility_90_minimal = f_prior_euro_mobility_90_minimal(ich_all),
  m_prior_euro_mobility_90_canonical = f_prior_euro_mobility_90_canonical(ich_all),
  m_prior_euro_mobility_180_minimal = f_prior_euro_mobility_180_minimal(ich_all),
  m_prior_euro_mobility_180_canonical = f_prior_euro_mobility_180_canonical(ich_all),
  m_prior_euro_mobility_365_minimal = f_prior_euro_mobility_365_minimal(ich_all),
  m_prior_euro_mobility_365_canonical = f_prior_euro_mobility_365_canonical(ich_all),
  m_prior_euro_selfcare_90_minimal = f_prior_euro_selfcare_90_minimal(ich_all),
  m_prior_euro_selfcare_90_canonical = f_prior_euro_selfcare_90_canonical(ich_all),
  m_prior_euro_selfcare_180_minimal = f_prior_euro_selfcare_180_minimal(ich_all),
  m_prior_euro_selfcare_180_canonical = f_prior_euro_selfcare_180_canonical(ich_all),
  m_prior_euro_selfcare_365_minimal = f_prior_euro_selfcare_365_minimal(ich_all),
  m_prior_euro_selfcare_365_canonical = f_prior_euro_selfcare_365_canonical(ich_all),
  m_prior_euro_usual_90_minimal = f_prior_euro_usual_90_minimal(ich_all),
  m_prior_euro_usual_90_canonical = f_prior_euro_usual_90_canonical(ich_all),
  m_prior_euro_usual_180_minimal = f_prior_euro_usual_180_minimal(ich_all),
  m_prior_euro_usual_180_canonical = f_prior_euro_usual_180_canonical(ich_all),
  m_prior_euro_usual_365_minimal = f_prior_euro_usual_365_minimal(ich_all),
  m_prior_euro_usual_365_canonical = f_prior_euro_usual_365_canonical(ich_all),
  m_prior_euro_pain_90_minimal = f_prior_euro_pain_90_minimal(ich_all),
  m_prior_euro_pain_90_canonical = f_prior_euro_pain_90_canonical(ich_all),
  m_prior_euro_pain_180_minimal = f_prior_euro_pain_180_minimal(ich_all),
  m_prior_euro_pain_180_canonical = f_prior_euro_pain_180_canonical(ich_all),
  m_prior_euro_pain_365_minimal = f_prior_euro_pain_365_minimal(ich_all),
  m_prior_euro_pain_365_canonical = f_prior_euro_pain_365_canonical(ich_all),
  m_prior_euro_anxiety_90_minimal = f_prior_euro_anxiety_90_minimal(ich_all),
  m_prior_euro_anxiety_90_canonical = f_prior_euro_anxiety_90_canonical(ich_all),
  m_prior_euro_anxiety_180_minimal = f_prior_euro_anxiety_180_minimal(ich_all),
  m_prior_euro_anxiety_180_canonical = f_prior_euro_anxiety_180_canonical(ich_all),
  m_prior_euro_anxiety_365_minimal = f_prior_euro_anxiety_365_minimal(ich_all),
  m_prior_euro_anxiety_365_canonical = f_prior_euro_anxiety_365_canonical(ich_all),


  ### Posterior Simulation ----
  m_posterior_mrs_90_minimal = f_posterior_mrs_90_minimal(ich_all),
  m_posterior_mrs_90_canonical = f_posterior_mrs_90_canonical(ich_all),
  m_posterior_mrs_180_minimal = f_posterior_mrs_180_minimal(ich_all),
  m_posterior_mrs_180_canonical = f_posterior_mrs_180_canonical(ich_all),
  m_posterior_mrs_365_minimal = f_posterior_mrs_365_minimal(ich_all),
  m_posterior_mrs_365_canonical = f_posterior_mrs_365_canonical(ich_all),
  m_posterior_euro_mobility_90_minimal = f_posterior_euro_mobility_90_minimal(ich_all),
  m_posterior_euro_mobility_90_canonical = f_posterior_euro_mobility_90_canonical(ich_all),
  m_posterior_euro_mobility_180_minimal = f_posterior_euro_mobility_180_minimal(ich_all),
  m_posterior_euro_mobility_180_canonical = f_posterior_euro_mobility_180_canonical(ich_all),
  m_posterior_euro_mobility_365_minimal = f_posterior_euro_mobility_365_minimal(ich_all),
  m_posterior_euro_mobility_365_canonical = f_posterior_euro_mobility_365_canonical(ich_all),
  m_posterior_euro_selfcare_90_minimal = f_posterior_euro_selfcare_90_minimal(ich_all),
  m_posterior_euro_selfcare_90_canonical = f_posterior_euro_selfcare_90_canonical(ich_all),
  m_posterior_euro_selfcare_180_minimal = f_posterior_euro_selfcare_180_minimal(ich_all),
  m_posterior_euro_selfcare_180_canonical = f_posterior_euro_selfcare_180_canonical(ich_all),
  m_posterior_euro_selfcare_365_minimal = f_posterior_euro_selfcare_365_minimal(ich_all),
  m_posterior_euro_selfcare_365_canonical = f_posterior_euro_selfcare_365_canonical(ich_all),
  m_posterior_euro_usual_90_minimal = f_posterior_euro_usual_90_minimal(ich_all),
  m_posterior_euro_usual_90_canonical = f_posterior_euro_usual_90_canonical(ich_all),
  m_posterior_euro_usual_180_minimal = f_posterior_euro_usual_180_minimal(ich_all),
  m_posterior_euro_usual_180_canonical = f_posterior_euro_usual_180_canonical(ich_all),
  m_posterior_euro_usual_365_minimal = f_posterior_euro_usual_365_minimal(ich_all),
  m_posterior_euro_usual_365_canonical = f_posterior_euro_usual_365_canonical(ich_all),
  m_posterior_euro_pain_90_minimal = f_posterior_euro_pain_90_minimal(ich_all),
  m_posterior_euro_pain_90_canonical = f_posterior_euro_pain_90_canonical(ich_all),
  m_posterior_euro_pain_180_minimal = f_posterior_euro_pain_180_minimal(ich_all),
  m_posterior_euro_pain_180_canonical = f_posterior_euro_pain_180_canonical(ich_all),
  m_posterior_euro_pain_365_minimal = f_posterior_euro_pain_365_minimal(ich_all),
  m_posterior_euro_pain_365_canonical = f_posterior_euro_pain_365_canonical(ich_all),
  m_posterior_euro_anxiety_90_minimal = f_posterior_euro_anxiety_90_minimal(ich_all),
  m_posterior_euro_anxiety_90_canonical = f_posterior_euro_anxiety_90_canonical(ich_all),
  m_posterior_euro_anxiety_180_minimal = f_posterior_euro_anxiety_180_minimal(ich_all),
  m_posterior_euro_anxiety_180_canonical = f_posterior_euro_anxiety_180_canonical(ich_all),
  m_posterior_euro_anxiety_365_minimal = f_posterior_euro_anxiety_365_minimal(ich_all),
  m_posterior_euro_anxiety_365_canonical = f_posterior_euro_anxiety_365_canonical(ich_all),

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
