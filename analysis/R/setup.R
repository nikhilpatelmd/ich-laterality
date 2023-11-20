options(
  tidyverse.quiet = TRUE
)

# libraries
library(tidyverse)
library(marginaleffects)
library(brms)
library(cmdstanr)
library(tidybayes)
library(parameters)
library(broom)
library(broom.mixed)
library(ggdist)
library(here)
library(dagitty)
library(ggdag)
library(MetBrewer)
library(patchwork)
library(tableone)
library(gt)
library(gtsummary)
library(gtExtras)
library(janitor)
library(scales) # Nicer labeling functions
library(ggmosaic) # Mosaic plots with ggplot

# Set some global Stan options
CHAINS <- 4
ITER <- 2000
WARMUP <- 1000
BAYES_SEED <- 160
options(mc.cores = parallel::detectCores(), brms.backend = "cmdstanr")

# seeds
set.seed(160)
BAYES_SEED <- 160

# plot stuff
theme_set(theme_minimal(base_family = "Figtree"))

# data import
data <- read_rds(here("data", "all.rds"), refhook = NULL)

data <- data |>
  filter(ich_location == "Basal Ganglia" | ich_location == "Thalamus" | ich_location == "Lobar") |>
  select(study, age, sex, race, ethnicity, time_symptoms_to_ed, sbp_baseline, ich_laterality, ich_location, nihss_baseline, gcs_baseline, ich_volume_baseline, ivh, ivh_volume_baseline, htn, hld, dm2, stroke, afib, tobacco, cad, neurosurgery_evac, neurosurgery_evac_day, evd, days_mechanical_ventilation, tracheostomy, peg, dnr_binary, dnr_day, dni_binary, dni_day, comfort_care_binary, comfort_care_day, mrs_90_01, mrs_90_02, mrs_90_03, mrs_90_04, mrs_90, hospital_los) |>
  drop_na(ich_laterality)

glimpse(data)

# # converting some columns to factors
# factors <- c("sex", "race", "euro_mobility_90", "euro_mobility_180", "euro_mobility_365", "euro_selfcare_90", "euro_selfcare_180", "euro_selfcare_365", "euro_usual_90", "euro_usual_180", "euro_usual_365", "euro_pain_90", "euro_pain_180", "euro_pain_365", "euro_anxiety_90", "euro_anxiety_180", "euro_anxiety_365", "location_90", "location_180", "location_365", "ethnicity", "stroke", "cad", "chf", "afib", "pvd", "hld", "dm2", "tobacco", "htn_meds", "dm2_meds", "ich_location", "ich_laterality", "ivh", "evd", "neurosurgery_evac", "tracheostomy", "peg", "dnr_binary", "dni_binary", "comfort_care_binary", "mrs_90", "mrs_180", "mrs_365", "euroqol_90_assessment_source", "euroqol_180_assessment_source", "euroqol_365_assessment_source", "euro_mobility_365_binary", "euro_selfcare_365_binary", "euro_usual_365_binary", "euro_pain_365_binary", "euro_anxiety_365_binary", "study", "enrolling_country", "treatment_group", "euro_mobility_90_binary", "euro_selfcare_90_binary", "euro_usual_90_binary", "euro_pain_90_binary", "euro_anxiety_90_binary", "cocaine", "anticoagulated", "etoh", "mrs_270", "location_30")

# data <- data |>
#   mutate(across(factors, factor))

data_erich_atach <- data |>
  filter(study == "ERICH" | study == "ATACH-2")

data_surgery_trials <- data |>
  filter(study == "MISTIE2" | study == "MISTIE-3" | study == "CLEAR III")
