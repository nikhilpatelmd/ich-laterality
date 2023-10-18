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
theme_set(theme_minimal(base_family = "Noah"))

# data import
data <- read_rds(here("data", "all.rds"), refhook = NULL)

data <- data |>
  filter(ich_location == "Basal Ganglia" | ich_location == "Thalamus" | ich_location == "Lobar") |>
  select(study, age, ich_laterality, ich_location, ich_volume_baseline, gcs_baseline, ivh, neurosurgery_evac, sbp_baseline, mrs_90_01, mrs_90_02, mrs_90_03, mrs_90_04, mrs_90) |>
  drop_na(ich_laterality)

data$ich_location <- fct_drop(data$ich_location)
data$ich_laterality <- fct_drop(data$ich_laterality)

data_erich_atach <- data |>
  filter(study == "ERICH" | study == "ATACH2")

data_surgery_trials <- data |>
  filter(study == "MISTIE2" | study == "MISTIE-3" | study == "CLEAR III")
