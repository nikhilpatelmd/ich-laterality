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

set.seed(4641) # From random.org

# R functions ----

source("R/packages.R")
source("R/data_cleaning.R")
source("R/exploratory_data_analysis.R")

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
  ich = filter_variables(selected_data),
  non_surgery_trials = ich |> filter(study == "ERICH" | study == "ATACH-2"),
  surgery_trials = ich |> filter(study == "MISTIE2" | study == "MISTIE-3" | study == "CLEAR III"),

  ## Exploratory data analysis ----
  table_1 = table_1_function(non_surgery_trials),
  table_2 = table_2_function(non_surgery_trials),

  ## Render report ----
  tar_quarto(
    report,
    "reports/aggressiveness.qmd"
  )
)
