library(targets)
library(tarchetypes)

# General pipeline settings ----

suppressPackageStartupMessages(library(brms))

# Taken from Andrew Heiss (https://github.com/andrewheiss/cautioning-canary/blob/master/_targets.R)
# Bayes-specific stuff
# Specify the number of cores and threads
#
# If using brms's default rstan backend, set mc.threads to NULL
#
#   - If using cmdstanr, you can use within-chain threading
#   - (https://cran.r-project.org/web/packages/brms/vignettes/brms_threading.html)
#   - On a four-core computer, use mc.cores = 2 and threads = threading(2)
#   - On an eight+-core computer, use mc.cores = 4 and threads = threading(2)
options(
  mc.cores = 4,
  mc.threads = 2,
  brms.backend = "cmdstanr"
)

options(
  tidyverse.quiet = TRUE,
  dplyr.summarise.inform = FALSE
)

set.seed(908)
BAYES_SEED <- 908



# R functions ----

source("R/packages.R")
source("R/data_cleaning.R")
source("R/exploratory_data_analysis.R")
source("R/dags.R")
source("R/models.R")
source("R/diagnostics.R")
source("R/posterior_results.R")


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
  model_parameters = model_parameters_function(ich_aggressive),
  prior_neurosurgery_minimal = prior_func_meta(model_parameters, "neurosurgery_minimal"),
  prior_neurosurgery_canonical = prior_func_meta(model_parameters, "neurosurgery_canonical"),
  prior_evd_minimal = prior_func_meta(model_parameters, "evd_minimal"),
  prior_evd_canonical = prior_func_meta(model_parameters, "evd_canonical"),
  prior_trach_minimal = prior_func_meta(model_parameters, "trach_minimal"),
  prior_trach_canonical = prior_func_meta(model_parameters, "trach_canonical"),
  prior_vent_minimal = prior_func_meta(model_parameters, "vent_minimal"),
  prior_vent_canonical = prior_func_meta(model_parameters, "vent_canonical"),
  prior_comfort_care_minimal = prior_func_meta(model_parameters, "comfort_care_minimal"),
  prior_comfort_care_canonical = prior_func_meta(model_parameters, "comfort_care_canonical"),
  prior_early_wlst_minimal = prior_func_meta(model_parameters, "early_wlst_minimal"),
  prior_early_wlst_canonical = prior_func_meta(model_parameters, "early_wlst_canonical"),
  prior_dnr_minimal = prior_func_meta(model_parameters, "dnr_minimal"),
  prior_dnr_canonical = prior_func_meta(model_parameters, "dnr_canonical"),

  ## Posterior Simulation ----
  posterior_neurosurgery_minimal = posterior_func_meta(model_parameters, "neurosurgery_minimal"),
  posterior_neurosurgery_canonical = posterior_func_meta(model_parameters, "neurosurgery_canonical"),
  posterior_evd_minimal = posterior_func_meta(model_parameters, "evd_minimal"),
  posterior_evd_canonical = posterior_func_meta(model_parameters, "evd_canonical"),
  posterior_trach_minimal = posterior_func_meta(model_parameters, "trach_minimal"),
  posterior_trach_canonical = posterior_func_meta(model_parameters, "trach_canonical"),
  posterior_vent_minimal = posterior_func_meta(model_parameters, "vent_minimal"),
  posterior_vent_canonical = posterior_func_meta(model_parameters, "vent_canonical"),
  posterior_comfort_care_minimal = posterior_func_meta(model_parameters, "comfort_care_minimal"),
  posterior_comfort_care_canonical = posterior_func_meta(model_parameters, "comfort_care_canonical"),
  posterior_early_wlst_minimal = posterior_func_meta(model_parameters, "early_wlst_minimal"),
  posterior_early_wlst_canonical = posterior_func_meta(model_parameters, "early_wlst_canonical"),
  posterior_dnr_minimal = posterior_func_meta(model_parameters, "dnr_minimal"),
  posterior_dnr_canonical = posterior_func_meta(model_parameters, "dnr_canonical"),

  ## Diagnostics ----
  neurosugery_minimal_diagnostics = posterior_diagnostics(posterior_neurosurgery_minimal),
  neurosugery_canonical_diagnostics = posterior_diagnostics(posterior_neurosurgery_canonical),
  evd_minimal_diagnostics = posterior_diagnostics(posterior_evd_minimal),
  evd_canonical_diagnostics = posterior_diagnostics(posterior_evd_canonical),
  trach_minimal_diagnostics = posterior_diagnostics(posterior_trach_minimal),
  trach_canonical_diagnostics = posterior_diagnostics(posterior_trach_canonical),
  vent_minimal_diagnostics = posterior_diagnostics(posterior_vent_minimal),
  vent_canonical_diagnostics = posterior_diagnostics(posterior_vent_canonical),
  comfort_care_minimal_diagnostics = posterior_diagnostics(posterior_comfort_care_minimal),
  comfort_care_canonical_diagnostics = posterior_diagnostics(posterior_comfort_care_canonical),
  early_wlst_minimal_diagnostics = posterior_diagnostics(posterior_early_wlst_minimal),
  early_wlst_canonical_diagnostics = posterior_diagnostics(posterior_early_wlst_canonical),
  dnr_minimal_diagnostics = posterior_diagnostics(posterior_dnr_minimal),
  dnr_canonical_diagnostics = posterior_diagnostics(posterior_dnr_canonical),

  ## Plots ----
  neurosugery_minimal_visuals = results_visual(posterior_neurosurgery_minimal, "Neurosurgical Intervention"),
  neurosugery_canonical_visuals = results_visual(posterior_neurosurgery_canonical, "Neurosurgical Intervention"),
  evd_minimal_visuals = results_visual(posterior_evd_minimal, "External Ventricular Drain"),
  evd_canonical_visuals = results_visual(posterior_evd_canonical, "External Ventricular Drain"),
  trach_minimal_visuals = results_visual(posterior_trach_minimal, "Tracheostomy"),
  trach_canonical_visuals = results_visual(posterior_trach_canonical, "Tracheostomy"),
  vent_minimal_visuals = results_visual(posterior_vent_minimal, "Days of Mechanical Ventilation"),
  vent_canonical_visuals = results_visual(posterior_vent_canonical, "Days of Mechanical Ventilation"),
  comfort_care_minimal_visuals = results_visual(posterior_comfort_care_minimal, "Comfort Care"),
  comfort_care_canonical_visuals = results_visual(posterior_comfort_care_canonical, "Comfort Care"),
  early_wlst_minimal_visuals = results_visual(posterior_early_wlst_minimal, "Early Comfort Care"),
  early_wlst_canonical_visuals = results_visual(posterior_early_wlst_canonical, "Early Comfort Care"),
  dnr_minimal_visuals = results_visual(posterior_dnr_minimal, "DNR Order"),
  dnr_canonical_visuals = results_visual(posterior_dnr_canonical, "DNR Order"),

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
