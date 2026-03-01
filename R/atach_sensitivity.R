# R/atach_sensitivity.R
# Sensitivity analysis adding ATACH-2-specific imaging confounders
# (midline_shift, hydrocephalus) that drive surgical decision-making
# but are unavailable in ERICH. Run on ich_atach only.

fit_atach_confounder_model <- function(
  data,
  random_effect_str = NULL,
  settings = model_setup("fast")
) {
  # Build random effect term — NULL means no clustering
  re_term <- if (!is.null(random_effect_str)) random_effect_str else ""

  formula <- as.formula(paste(
    "neurosurgery_evac ~ ich_laterality +",
    "ich_location + age + gcs_baseline + ich_volume_baseline + ivh +",
    "midline_shift + hydrocephalus +", # The new confounders
    "ich_laterality:ich_location", # Keep the laterality×location interaction
    if (nchar(re_term) > 0) paste("+", re_term) else ""
  ))

  brm(
    formula,
    family = bernoulli(link = "logit"),
    data = data,
    prior = c(
      set_prior("normal(-7, 0.35)", class = "Intercept"),
      set_prior("normal(0, 0.5)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    control = list(adapt_delta = 0.99)
  )
}