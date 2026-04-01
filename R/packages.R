suppressPackageStartupMessages({
  # Core tidyverse & data manipulation
  library(tidyverse)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(stringr)
  library(janitor)
  library(glue)

  # Bayesian modeling
  library(cmdstanr)
  library(brms)
  library(tidybayes)
  library(bayesplot)
  library(marginaleffects)

  # Imputation & missing data
  library(mice)
  library(naniar)

  # Visualization
  library(ggplot2)
  library(ggdist)
  library(ggridges)
  library(ggtext)
  library(patchwork)
  library(scales)
  library(viridis)
  library(monochromeR)
  library(systemfonts)
  library(showtext)

  # DAGs
  library(dagitty)
  library(ggdag)

  # Tables
  library(gt)
  library(gtsummary)
  library(smd)

  # Tidying model output
  library(broom)
  library(broom.mixed)

  # Utility
  library(rlang)
})
