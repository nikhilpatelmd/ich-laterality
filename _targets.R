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

# =========================================================================
# SETUP & CONFIGURATION
# =========================================================================
plan(callr)

tar_option_set(
  garbage_collection = TRUE,
  memory = "transient"
)

options(brms.backend = "cmdstanr")
options(tidyverse.quiet = TRUE, dplyr.summarise.inform = FALSE)

# Source all R functions
tar_source("R/")


# =========================================================================
# DATA PREPARATION & EXPLORATORY DATA ANALYSIS (EDA)
# =========================================================================
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

  tar_target(
    ich_imputed_file,
    command = {
      imp_obj <- f_imputed(ich_aggressive, n_imputes = 20)
      path <- "data/proc/ich_imputed.rds"
      dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
      saveRDS(imp_obj, path)
      return(path)
    },
    format = "file"
  ),

  tar_target(
    ich_atach_imputed_file,
    command = {
      imp_obj <- f_imputed_atach(ich_atach, n_imputes = 20)
      path <- "data/proc/ich_atach_imputed.rds"
      dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
      saveRDS(imp_obj, path)
      return(path)
    },
    format = "file"
  ),

  tar_target(
    imputed_visualizations,
    f_plot_imputations_detailed(readRDS(ich_imputed_file))
  ),

  tar_target(dag_neurosurgery, f_neurosurgery_dag()),
  tar_target(dag_outcomes, outcomes_dag_function()),
  tar_target(settings, model_setup())
)

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
    ),
    tar_target(
      name = missing_data_by_file,
      command = {
        dir.create(
          "figures/missing_data",
          showWarnings = FALSE,
          recursive = TRUE
        )
        path <- file.path(
          "figures/missing_data",
          paste0("missing_pct_by_", variable_name, ".pdf")
        )
        ggsave(
          filename = path,
          plot = missing_data_by,
          width = 8,
          height = 6,
          units = "in",
          device = cairo_pdf
        )
        path
      },
      format = "file"
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
    ),
    tar_target(
      name = missingness_check_file,
      command = {
        dir.create(
          "figures/missing_data",
          showWarnings = FALSE,
          recursive = TRUE
        )
        path <- file.path(
          "figures/missing_data",
          paste0(
            "shadow_plot_",
            plotting_variable,
            "_by_",
            missing_variable,
            ".pdf"
          )
        )
        ggsave(
          filename = path,
          plot = missingness_check,
          width = 8,
          height = 6,
          units = "in",
          device = cairo_pdf
        )
        path
      },
      format = "file"
    )
  )
)


# =========================================================================
# CANDIDATE MODEL GRIDS
#
# The grid definitions encode the scientific design of the pipeline. The
# key principle is that each sensitivity analysis varies exactly one thing
# relative to the primary (MICE-imputed, neutral-prior, adjusted) analysis:
#
#   - Main models:        all 4 priors × 2 adjustment sets (full factorial —
#                         both dimensions have independent scientific meaning)
#   - Prior predictive:   all 4 priors × adjusted only (checking prior
#                         plausibility; adjustment set doesn't change the
#                         intercept or laterality priors being examined)
#   - Complete-case sens: neutral prior × adjusted only (one thing changes:
#                         imputation strategy; everything else held constant)
#   - ATACH-2 sens:       neutral prior × adjusted only (one thing changes:
#                         population; same logic)
#   - Interaction models: neutral prior × adjusted (exploratory, not primary)
# =========================================================================

aggressive_grid <- tibble::tribble(
  ~outcome_col          , ~family                          , ~int_mean , ~int_sd , ~complexity ,
  "neurosurgery_evac"   , quote(bernoulli(link = "logit")) ,        -7 , 0.35    , "complex"   ,
  "evd"                 , quote(bernoulli(link = "logit")) ,         0 , 0.5     , "complex"   ,
  "dnr_binary"          , quote(bernoulli(link = "logit")) ,        -5 , 0.5     , "complex"   ,
  "comfort_care_binary" , quote(bernoulli(link = "logit")) ,        -5 , 0.5     , "complex"   ,
  "early_wlst"          , quote(bernoulli(link = "logit")) ,         0 , 0.5     , "complex"   ,
  "tracheostomy"        , quote(bernoulli(link = "logit")) ,       -15 , 0.5     , "complex"
)

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

combined_base <- bind_rows(aggressive_grid, functional_grid)

# ── Main model grids: full 4-prior × 2-adjustment-set factorial ───────────────
complete_grid <- tidyr::crossing(
  combined_base,
  prior_scenario = c("neutral", "left", "right", "flat"),
  adjustment_set = c("minimal", "adjusted")
)

grid_fast <- complete_grid |> filter(complexity == "fast")
grid_complex <- complete_grid |> filter(complexity == "complex")

grid_ventilation <- tibble::tribble(
  ~outcome_col                  , ~family                                        , ~int_mean , ~int_sd , ~complexity ,
  "days_mechanical_ventilation" , quote(zero_inflated_negbinomial(link = "log")) ,         0 , 0.5     , "complex"
) |>
  tidyr::crossing(
    prior_scenario = c("neutral", "left", "right", "flat"),
    adjustment_set = c("minimal", "adjusted")
  )

grid_vas <- tibble::tibble(
  outcome_col = "euro_vas_90",
  family = list(quote(zero_one_inflated_beta()))
) |>
  tidyr::crossing(
    prior_scenario = c("neutral", "left", "right", "flat"),
    adjustment_set = c("minimal", "adjusted")
  )

# ── Prior predictive grids: all 4 priors × adjusted only ─────────────────────
# The adjustment set does not affect the intercept or laterality priors being
# checked, so running minimal-adjusted prior predictives is redundant.
grid_priors <- complete_grid |> filter(adjustment_set == "adjusted")
grid_priors_fast <- grid_priors |> filter(complexity == "fast")
grid_priors_complex <- grid_priors |> filter(complexity == "complex")

grid_priors_ventilation <- tibble::tribble(
  ~outcome_col                  , ~family                                        , ~int_mean , ~int_sd , ~complexity ,
  "days_mechanical_ventilation" , quote(zero_inflated_negbinomial(link = "log")) ,         0 , 0.5     , "complex"
) |>
  tidyr::crossing(
    prior_scenario = c("neutral", "left", "right", "flat"),
    adjustment_set = "adjusted"
  )

grid_priors_vas <- tibble::tibble(
  outcome_col = "euro_vas_90",
  family = list(quote(zero_one_inflated_beta()))
) |>
  tidyr::crossing(
    prior_scenario = c("neutral", "left", "right", "flat"),
    adjustment_set = "adjusted"
  )

# ── Complete-case sensitivity grids: neutral prior × adjusted only ────────────
# These models answer one question: does MICE imputation materially change
# results vs. a complete-case approach? Only neutral-prior + adjusted models
# are needed — varying the prior or adjustment set simultaneously would blur
# the interpretation of the sensitivity analysis.
grid_fast_sens <- combined_base |>
  filter(complexity == "fast") |>
  tidyr::crossing(prior_scenario = "neutral", adjustment_set = "adjusted")

grid_complex_sens <- combined_base |>
  filter(complexity == "complex") |>
  tidyr::crossing(prior_scenario = "neutral", adjustment_set = "adjusted")

grid_vas_sens <- tibble::tibble(
  outcome_col = "euro_vas_90",
  family = list(quote(zero_one_inflated_beta()))
) |>
  tidyr::crossing(prior_scenario = "neutral", adjustment_set = "adjusted")

# ── Other sensitivity grids ───────────────────────────────────────────────────
# grid_atach_sens and grid_interactions follow the same single-dimension logic:
# each varies only population (ATACH-2 only) or model structure (interaction
# term), holding prior and adjustment constant at the primary analysis values.
grid_atach_sens <- aggressive_grid |>
  filter(outcome_col == "neurosurgery_evac") |>
  tidyr::crossing(prior_scenario = "neutral", adjustment_set = "adjusted")

grid_interactions <- tibble(
  outcome_col = "neurosurgery_evac",
  interaction_var = c("ich_location", "study"),
  family = rep(list(quote(bernoulli(link = "logit"))), 2),
  prior_scenario = "neutral",
  adjustment_set = "adjusted"
)

# ── Table iteration scenarios ─────────────────────────────────────────────────
# Main and prior tables iterate over all 4 priors; the sensitivity table only
# needs neutral, since the complete-case models were only fit under that prior.
table_scenarios <- tibble(scenario = c("neutral", "left", "right", "flat"))
table_scenarios_sens <- tibble(scenario = "neutral")


# ── Figure scenarios metadata (binary outcomes) ────────────────────────────────
# model_key is pre-computed here rather than constructed inside the tar_map
# expression — tar_map substitutes column values as bare symbols, so building
# strings with paste0() inside the expression is unreliable.
figure_scenarios <- tibble::tribble(
  ~outcome_col                                                                                                                                                                                                   , ~outcome_label               , ~x_limits      , ~covariate_caption ,

  "neurosurgery_evac"                                                                                                                                                                                            , "Neurosurgical Intervention" , list(c(0, 20)) ,
  "Models were adjusted for ICH location, age, admission Glasgow Coma Scale score, ICH volume, intraventricular hemorrhage, an ICH laterality-by-location interaction, and a random intercept for study center." ,

  "evd"                                                                                                                                                                                                          , "EVD Placement"              , list(c(0, 20)) ,
  "Models were adjusted for ICH location, age, admission Glasgow Coma Scale score, ICH volume, intraventricular hemorrhage, an ICH laterality-by-location interaction, and a random intercept for study center." ,

  "tracheostomy"                                                                                                                                                                                                 , "Tracheostomy"               , list(c(0, 30)) ,
  "Models were adjusted for ICH location, age, admission Glasgow Coma Scale score, ICH volume, intraventricular hemorrhage, neurosurgical intervention, and a random intercept for study center."                ,

  "comfort_care_binary"                                                                                                                                                                                          , "Comfort Care"               , list(c(0, 50)) ,
  "Models were adjusted for ICH location, age, admission Glasgow Coma Scale score, ICH volume, intraventricular hemorrhage, and a random intercept for study center."                                            ,

  "early_wlst"                                                                                                                                                                                                   , "Early WLST"                 , list(c(0, 50)) ,
  "Models were adjusted for ICH location, age, admission Glasgow Coma Scale score, ICH volume, intraventricular hemorrhage, and a random intercept for study center."                                            ,

  "dnr_binary"                                                                                                                                                                                                   , "DNR Order"                  , list(c(0, 70)) ,
  "Models were adjusted for ICH location, age, admission Glasgow Coma Scale score, ICH volume, intraventricular hemorrhage, and a random intercept for study center."
)

figure_values <- tidyr::crossing(
  figure_scenarios,
  prior_scenario = c("neutral", "left", "right", "flat")
) |>
  mutate(
    model_key = paste0(
      "model_main_",
      outcome_col,
      "_",
      prior_scenario,
      "_adjusted"
    )
  )

# ── Prior sensitivity figure metadata ─────────────────────────────────────────
figure_sensitivity_scenarios <- figure_scenarios |>
  mutate(
    key_neutral = paste0("model_main_", outcome_col, "_neutral_adjusted"),
    key_left = paste0("model_main_", outcome_col, "_left_adjusted"),
    key_right = paste0("model_main_", outcome_col, "_right_adjusted"),
    key_flat = paste0("model_main_", outcome_col, "_flat_adjusted")
  )


# ── mRS figure scenarios ───────────────────────────────────────────────────────
mrs_figure_scenarios <- tibble::tibble(
  outcome_col    = "mrs_90",
  prior_scenario = "neutral",
  adjustment_set = "adjusted",
  model_key      = "model_main_mrs_90_neutral_adjusted",
  figure_key     = "main_neutral_adjusted"
)

# ── EuroQOL figure scenarios ───────────────────────────────────────────────────
euro_figure_scenarios <- tibble::tibble(
  outcome_col = c(
    "euro_mobility_90", "euro_selfcare_90", "euro_usual_90",
    "euro_pain_90",     "euro_anxiety_90"
  ),
  dimension = c("mobility", "selfcare", "usual", "pain", "anxiety")
) |>
  mutate(
    prior_scenario = "neutral",
    adjustment_set = "adjusted",
    model_key      = paste0("model_main_", outcome_col, "_neutral_adjusted"),
    figure_key     = paste0("main_", dimension, "_neutral_adjusted")
  )

# ── VAS figure scenarios ───────────────────────────────────────────────────────
vas_figure_scenarios <- tibble::tibble(
  outcome_col    = "euro_vas_90",
  prior_scenario = "neutral",
  adjustment_set = "adjusted",
  model_key      = "model_main_euro_vas_90_neutral_adjusted",
  figure_key     = "main_neutral_adjusted"
)


# =========================================================================
# PRIORS & PRIOR PREDICTIVE CHECKS
# =========================================================================
# All 4 priors × adjusted only — the adjustment set does not affect the
# intercept or laterality priors, so minimal-adjusted prior predictives
# would be redundant and expensive.
map_priors <- tar_map(
  values = grid_priors,
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
      sample_prior = "only",
      settings = model_setup("fast"),
      random_effect_str = "(1 | study)"
    )),
    deployment = "worker"
  )
)

map_priors_ventilation <- tar_map(
  values = grid_priors_ventilation,
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

map_priors_vas <- tar_map(
  values = grid_priors_vas,
  names = c("outcome_col", "prior_scenario", "adjustment_set"),
  unlist = FALSE,
  tar_target(
    model_prior,
    list(fit_vas_zoib_prior_only(
      data = ich_aggressive,
      use_imputation = FALSE,
      prior_scenario = prior_scenario,
      adjustment_set = adjustment_set,
      settings = model_setup("fast"),
      random_effect_str = "(1 | study)",
      sample_prior = "only"
    )),
    deployment = "worker"
  )
)

t_combine_priors <- list(
  tar_combine(
    all_prior_models,
    map_priors,
    map_priors_ventilation,
    map_priors_vas,
    command = c(!!!.x)
  )
)

# ── Prior predictive check forest plots (supplement) ──────────────────────────
# Each figure receives the full all_prior_models list and returns a single plot
# covering all outcomes × all 4 prior scenarios. Both targets depend on
# all_prior_models (via tar_combine), so they only build once all prior-only
# models have finished sampling.
t_presentation_ppc <- list(
  tar_target(
    figure_ppc_forest_aggressive,
    make_ppc_forest_aggressive(all_prior_models),
    deployment = "main"
  ),

  tar_target(
    figure_ppc_forest_aggressive_file,
    {
      dir.create("figures/supplement", showWarnings = FALSE, recursive = TRUE)
      path <- "figures/supplement/sfig_ppc_forest_aggressive.pdf"
      ggsave(
        filename = path,
        plot = figure_ppc_forest_aggressive,
        width = 9,
        height = 7,
        units = "in",
        device = cairo_pdf
      )
      path
    },
    format = "file",
    deployment = "main"
  ),

  tar_target(
    figure_ppc_forest_functional,
    make_ppc_forest_functional(all_prior_models, ich_aggressive),
    deployment = "main"
  ),

  tar_target(
    figure_ppc_forest_functional_file,
    {
      dir.create("figures/supplement", showWarnings = FALSE, recursive = TRUE)
      path <- "figures/supplement/sfig_ppc_forest_functional.pdf"
      ggsave(
        filename = path,
        plot = figure_ppc_forest_functional,
        width = 9,
        height = 8.5,
        units = "in",
        device = cairo_pdf
      )
      path
    },
    format = "file",
    deployment = "main"
  )
)


# =========================================================================
# FITTING THE MODELS
# =========================================================================

# ── Primary analysis: MICE-imputed, all 4 priors × 2 adjustment sets ──────────
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

map_main_ventilation <- tar_map(
  values = grid_ventilation,
  names = c("outcome_col", "prior_scenario", "adjustment_set"),
  unlist = FALSE,
  tar_target(
    model_main,
    list(fit_ventilation_zinb(
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

# ── Complete-case sensitivity: neutral prior × adjusted only ──────────────────
# Each of these maps varies exactly one thing vs. the primary analysis:
# complete cases rather than MICE-imputed data.
#
# Note on ventilation: days_mechanical_ventilation is structurally missing in
# ERICH (not collected), so a complete-case sensitivity is not meaningful —
# the primary ventilation model already uses complete cases. For that reason
# map_main_ventilation (neutral + adjusted) is included in all_sens_models
# below so tables have a consistent row, but no new model is fit.
map_sens_fast <- tar_map(
  values = grid_fast_sens,
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
  values = grid_complex_sens,
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

map_sens_vas <- tar_map(
  values = grid_vas_sens,
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

# ── ATACH-2 population restriction sensitivity: neutral prior × adjusted ───────
map_atach_sens <- tar_map(
  values = grid_atach_sens,
  names = "outcome_col",
  unlist = FALSE,
  tar_target(
    model_atach_base,
    list(fit_laterality_model(
      data = ich_atach_imputed_file,
      outcome_col = outcome_col,
      family = family,
      prior_scenario = prior_scenario,
      adjustment_set = adjustment_set,
      int_mean = int_mean,
      int_sd = int_sd,
      sample_prior = "no",
      settings = model_setup("fast"),
      use_imputation = TRUE,
      random_effect_str = NULL
    )),
    deployment = "worker"
  ),
  tar_target(
    model_atach_site,
    list(fit_laterality_model(
      data = ich_atach_imputed_file,
      outcome_col = outcome_col,
      family = family,
      prior_scenario = prior_scenario,
      adjustment_set = adjustment_set,
      int_mean = int_mean,
      int_sd = int_sd,
      sample_prior = "no",
      settings = model_setup("fast"),
      use_imputation = TRUE,
      random_effect_str = "(1 | site_id)"
    )),
    deployment = "worker"
  )
)

# ── Interaction models: neutral prior × adjusted ───────────────────────────────
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

t_combine_fits <- list(
  tar_combine(
    all_main_models,
    map_main_fast,
    map_main_complex,
    map_main_ventilation,
    map_main_vas,
    command = c(!!!.x)
  ),
  # Ventilation is included from map_main_ventilation (neutral + adjusted) because
  # it is structurally missing in ERICH — there is no complete-case analogue to fit.
  # Its keys will have the model_main_ prefix; subset_models_for_table3() must
  # handle this mixed prefix when ventilation rows are extracted.
  tar_combine(
    all_sens_models,
    map_sens_fast,
    map_sens_complex,
    map_sens_vas,
    map_main_ventilation,
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
  )
)


# =========================================================================
# EVALUATING FIT & DIAGNOSTICS
# =========================================================================
t_diagnostics <- list(
  tar_target(main_pp_checks, purrr::map(all_main_models, f_general_pp_check)),
  tar_target(sens_pp_checks, purrr::map(all_sens_models, f_general_pp_check)),

  tar_target(
    main_diagnostics,
    purrr::map(all_main_models, f_posterior_diagnostics)
  ),
  tar_target(
    sens_diagnostics,
    purrr::map(all_sens_models, f_posterior_diagnostics)
  )
)


# =========================================================================
# PRESENTATION OF RESULTS (Tables & Figures)
# =========================================================================
t_presentation_misc <- list(
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
            broom.mixed::tidy(.x, effects = "fixed", conf.int = TRUE) |>
              filter(grepl("laterality", term))
          }
        )
      ) |>
      tidyr::unnest(estimates) |>
      select(model_name, term, estimate, conf.low, conf.high)
  ),
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
            broom.mixed::tidy(.x, effects = "fixed", conf.int = TRUE) |>
              filter(grepl(":", term))
          }
        )
      ) |>
      tidyr::unnest(estimates) |>
      select(model_name, term, estimate, conf.low, conf.high)
  )
)

t_presentation_subgroups <- list(
  tar_target(
    table_subgroups,
    table_subgroups_function(
      data = ich_aggressive,
      loc_model = all_interaction_models[["model_interaction_ich_location"]],
      study_model = all_interaction_models[["model_interaction_study"]]
    )
  ),
  tar_target(
    table_2_atach,
    table_2_atach_function(
      data = ich_atach,
      base_model = all_site_sens_models[["model_atach_base_neurosurgery_evac"]],
      site_model = all_site_sens_models[["model_atach_site_neurosurgery_evac"]]
    )
  )
)

# ── Supplement DAG figures ─────────────────────────────────────────────────────
t_presentation_dags <- list(
  tar_target(
    figure_dag_neurosurgery,
    make_neurosurgery_dag_figure(dag_neurosurgery)
  ),

  tar_target(
    figure_dag_neurosurgery_file,
    {
      dir.create("figures/supplement", showWarnings = FALSE, recursive = TRUE)
      path <- "figures/supplement/sfig_dag_neurosurgery.pdf"
      ggsave(
        filename = path,
        plot = figure_dag_neurosurgery,
        device = cairo_pdf,
        width = 9,
        height = 9,
        units = "in"
      )
      path
    },
    format = "file"
  ),

  tar_target(
    figure_dag_outcomes,
    make_outcomes_dag_figure(dag_outcomes)
  ),

  tar_target(
    figure_dag_outcomes_file,
    {
      dir.create("figures/supplement", showWarnings = FALSE, recursive = TRUE)
      path <- "figures/supplement/sfig_dag_outcomes.pdf"
      ggsave(
        filename = path,
        plot = figure_dag_outcomes,
        device = cairo_pdf,
        width = 11,
        height = 10,
        units = "in"
      )
      path
    },
    format = "file"
  )
)

# ── Posterior probability figures (all binary outcomes × all priors) ──────────
map_posterior_figures <- tar_map(
  values = figure_values,
  names = c(outcome_col, prior_scenario),

  tar_target(
    figure_posterior,
    make_posterior_prob_figure(
      model = all_main_models[[model_key]],
      outcome_label = outcome_label,
      covariate_caption = covariate_caption,
      x_limits = unlist(x_limits)
    ),
    deployment = "main"
  ),

  tar_target(
    figure_posterior_file,
    {
      dir.create("figures", showWarnings = FALSE, recursive = TRUE)
      path <- file.path(
        "figures",
        paste0("figure_posterior_", outcome_col, "_", prior_scenario, ".pdf")
      )
      ggsave(
        filename = path,
        plot = figure_posterior,
        width = 9,
        height = 9,
        units = "in",
        device = cairo_pdf
      )
      path
    },
    format = "file",
    deployment = "main"
  )
)

# ── Prior sensitivity figures (all binary outcomes, all 4 priors overlaid) ────
map_sensitivity_figures <- tar_map(
  values = figure_sensitivity_scenarios,
  names = outcome_col,

  tar_target(
    figure_sensitivity,
    make_prior_sensitivity_figure(
      models_by_prior = list(
        neutral = all_main_models[[key_neutral]],
        left = all_main_models[[key_left]],
        right = all_main_models[[key_right]],
        flat = all_main_models[[key_flat]]
      ),
      outcome_label = outcome_label,
      covariate_caption = covariate_caption,
      x_limits = unlist(x_limits)
    ),
    deployment = "main"
  ),

  tar_target(
    figure_sensitivity_file,
    {
      dir.create("figures", showWarnings = FALSE, recursive = TRUE)
      path <- file.path(
        "figures",
        paste0("figure_sensitivity_", outcome_col, ".pdf")
      )
      ggsave(
        filename = path,
        plot = figure_sensitivity,
        width = 9,
        height = 9,
        units = "in",
        device = cairo_pdf
      )
      path
    },
    format = "file",
    deployment = "main"
  )
)

# ── mRS figure (neutral + adjusted, main imputed model) ───────────────────────
map_mrs_figures <- tar_map(
  values = mrs_figure_scenarios,
  names  = c("prior_scenario", "adjustment_set"),

  tar_target(
    figure_mrs,
    make_mrs_figure(all_main_models[[model_key]]),
    deployment = "main"
  ),

  tar_target(
    figure_mrs_file,
    {
      dir.create("figures/mrs", showWarnings = FALSE, recursive = TRUE)
      path <- file.path(
        "figures/mrs",
        paste0("figure_mrs_", figure_key, ".pdf")
      )
      ggsave(
        filename = path, plot = figure_mrs,
        width = 14, height = 9, units = "in", device = cairo_pdf
      )
      path
    },
    format = "file", deployment = "main"
  )
)

# ── EuroQOL figures (neutral + adjusted, main imputed models) ─────────────────
map_euro_figures <- tar_map(
  values = euro_figure_scenarios,
  names  = c("outcome_col", "prior_scenario", "adjustment_set"),

  tar_target(
    figure_euro,
    make_euro_figure(all_main_models[[model_key]], dimension),
    deployment = "main"
  ),

  tar_target(
    figure_euro_file,
    {
      dir.create("figures/euro", showWarnings = FALSE, recursive = TRUE)
      path <- file.path(
        "figures/euro",
        paste0("figure_euro_", figure_key, ".pdf")
      )
      ggsave(
        filename = path, plot = figure_euro,
        width = 14, height = 9, units = "in", device = cairo_pdf
      )
      path
    },
    format = "file", deployment = "main"
  )
)

# ── VAS figure (neutral + adjusted, main imputed model) ───────────────────────
map_vas_figures <- tar_map(
  values = vas_figure_scenarios,
  names  = c("prior_scenario", "adjustment_set"),

  tar_target(
    figure_vas,
    make_vas_figure(all_main_models[[model_key]], ich_aggressive),
    deployment = "main"
  ),

  tar_target(
    figure_vas_file,
    {
      dir.create("figures/euro", showWarnings = FALSE, recursive = TRUE)
      path <- file.path(
        "figures/euro",
        paste0("figure_vas_", figure_key, ".pdf")
      )
      ggsave(
        filename = path, plot = figure_vas,
        width = 10, height = 10, units = "in", device = cairo_pdf
      )
      path
    },
    format = "file", deployment = "main"
  )
)

# ── Tables ────────────────────────────────────────────────────────────────────
t_table1 <- tar_target(
  table_1,
  table_1_function(ich_aggressive)
)

# Main and prior tables iterate over all 4 prior scenarios.
# Sensitivity tables iterate over neutral only, matching the models that exist.
map_table2 <- tar_map(
  values = table_scenarios,
  tar_target(
    table_2,
    table_2_function(
      x = ich_aggressive,
      models = subset_models_for_table2(
        all_main_models,
        scenario,
        prefix = "model_main_"
      )
    )
  )
)

map_table2_sens <- tar_map(
  values = table_scenarios_sens,
  tar_target(
    table_2_sens,
    table_2_function(
      x = ich_aggressive,
      models = subset_models_for_table2(
        all_sens_models,
        scenario,
        prefix = "model_sens_"
      )
    )
  )
)

map_table2_priors <- tar_map(
  values = table_scenarios,
  tar_target(
    table_2_priors,
    table_2_priors_function(
      models = subset_prior_models_for_table2(all_prior_models, scenario)
    )
  )
)

map_table3 <- tar_map(
  values = table_scenarios,
  tar_target(
    table_3,
    table_3_function(
      x = ich_aggressive,
      models = subset_models_for_table3(
        all_main_models,
        scenario,
        prefix = "model_main_"
      ),
      is_prior = FALSE
    )
  )
)

map_table3_sens <- tar_map(
  values = table_scenarios_sens,
  tar_target(
    table_3_sens,
    table_3_function(
      x = ich_aggressive,
      models = subset_models_for_table3(
        all_sens_models,
        scenario,
        prefix = "model_sens_"
      ),
      is_prior = FALSE
    )
  )
)

map_table3_priors <- tar_map(
  values = table_scenarios,
  tar_target(
    table_3_priors,
    table_3_function(
      x = ich_aggressive,
      models = subset_models_for_table3(
        all_prior_models,
        scenario,
        prefix = "model_prior_"
      ),
      is_prior = TRUE
    )
  )
)


# =========================================================================
# FINAL PIPELINE ASSEMBLY
# =========================================================================
list(
  t_data,
  t_missing,

  # Priors
  map_priors,
  map_priors_ventilation,
  map_priors_vas,
  t_combine_priors,
  t_presentation_ppc,

  # Model Fitting
  map_main_fast,
  map_main_complex,
  map_main_ventilation,
  map_main_vas,
  map_sens_fast,
  map_sens_complex,
  map_sens_vas,
  map_atach_sens,
  map_interactions,
  t_combine_fits,

  # Diagnostics
  t_diagnostics,

  # Presentation
  t_presentation_misc,
  t_presentation_subgroups,
  t_presentation_dags,
  map_posterior_figures,
  map_sensitivity_figures,
  map_mrs_figures,
  map_euro_figures,
  map_vas_figures,
  t_table1,
  map_table2,
  map_table2_sens,
  map_table2_priors,
  map_table3,
  map_table3_sens,
  map_table3_priors
)