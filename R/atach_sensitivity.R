# --- 1. Dedicated ATACH Imputation Function ---
f_imputed_atach <- function(data, n_imputes = 20, seed = 1) {
  fixed_vars <- c(
    "neurosurgery_evac",
    "ich_laterality",
    "ich_location",
    "age",
    "ivh",
    "gcs_baseline",
    "ich_volume_baseline",
    "htn",
    "evd",
    "time_symptoms_to_ed",
    "site_id",
    "comfort_care_binary",
    "early_wlst",
    "dnr_binary",
    "tracheostomy",
    "days_mechanical_ventilation"
  )

  imp_data <- data |>
    select(
      any_of(fixed_vars),
      starts_with(c("mrs", "euro")),
      -ends_with(c("180", "365")),
      -ends_with(c("01", "02", "03", "04"))
    ) |>
    mutate(across(
      any_of(c(
        "neurosurgery_evac",
        "ivh",
        "htn",
        "evd",
        "ich_location",
        "ich_laterality",
        "comfort_care_binary",
        "early_wlst",
        "dnr_binary",
        "tracheostomy",
        "site_id" # Ensure site_id is a factor
      )),
      as.factor
    ))

  ini <- mice(imp_data, maxit = 0)

  meth <- tibble(variable = names(ini$method)) |>
    mutate(
      method = case_when(
        # THE FIX: Tell mice to skip imputing site_id
        variable %in% c("site_id", "days_mechanical_ventilation") ~ "",
        str_starts(variable, "euro|mrs") ~ "pmm",
        variable %in%
          c(
            "age",
            "gcs_baseline",
            "ich_volume_baseline",
            "time_symptoms_to_ed"
          ) ~ "pmm",
        variable %in%
          c(
            "neurosurgery_evac",
            "ivh",
            "htn",
            "evd",
            "comfort_care_binary",
            "early_wlst",
            "dnr_binary",
            "tracheostomy"
          ) ~ "logreg",
        variable %in% c("ich_location", "ich_laterality") ~ "polyreg",
        TRUE ~ ""
      )
    ) |>
    deframe()

  imp <- mice(
    imp_data,
    method = meth,
    m = n_imputes,
    maxit = 20,
    seed = seed,
    print = FALSE,
    ridge = 0.01,
    threshold = 0.999
  )

  return(imp)
}
