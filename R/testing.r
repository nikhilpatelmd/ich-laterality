Steps needed to build a bunch of prior models:

# Aggressive Care

outcomes: neurosurgery, evd, comfort_care, mech_vent, trach, and peg

All but mech_vent are logistic models

1) define formulas (simple and canonical)
2) define priors (will this be different for each?)
3) define data (will be the same for all)
4) define family (will be the same except for mech_vent)

## formulas

formulas_aggressive <- list(
  neurosurgery_simple = neurosurgery_evac ~ ich_laterality,
  neurosurgery_canonical = neurosurgery_evac ~ ich_laterality + age + hypertension + stroke + gcs_baseline + ich_location + ich_volume + ivh

)

is.atomic(formulas_aggressive[["neurosurgery_simple"]])

prior_f(prior_arguments)

str(prior_arguments)
### neurosurgery

neurosurgery_evac ~ ich_laterality

neurosurgery_evac ~ ich_laterality + age + hypertension + stroke + gcs_baseline + ich_location + ich_volume + ivh

### evd

evd ~ ich_laterality

evd ~ ich_laterality + age + htn + stroke + gcs_baseline + ich_location + ich_volume_baseline + ivh

### trach

tracheostomy ~ ich_laterality

tracheostomy ~ age + hypertension + stroke + gcs_baseline + ich_location + ich_volume + ivh

### peg

peg ~ ich_laterality

peg ~ age + hypertension + stroke + gcs_baseline + ich_location + ich_volume + ivh

### vent

days_mechanical_ventilation ~ ich_laterality

days_mechanical_ventilation ~ age + hypertension + stroke + gcs_baseline + ich_location + ich_volume + ivh


prior_arguments <- list(
  formula = neurosurgery_evac ~ ich_laterality,
  priors = c(
    set_prior("normal(-2.2, 0.7)", class = "Intercept"),
    set_prior("normal(0, 1)", class = "b")
  ),
  data = ich_aggressive,
  family = bernoulli(link = "logit")
)