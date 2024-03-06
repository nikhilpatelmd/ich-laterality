table_1_function <- function(x) {


  table_one_vars <- c(
    "age",
    "sex",
    "race",
    "ethnicity",
    "time_symptoms_to_ed",
    "sbp_baseline",
    "nihss_baseline",
    "gcs_baseline",
    "ich_laterality",
    "htn",
    "dm2",
    "stroke",
    "tobacco",
    "ich_location",
    "ich_volume_baseline",
    "ivh",
    "study"
  )

  x |>
    select(all_of(table_one_vars)) |>
    tbl_summary(
      by = ich_laterality,
      missing = "no",
      label = list(
        age ~ "Age",
        sex ~ "Sex",
        race ~ "Race",
        ethnicity ~ "Ethnicity",
        time_symptoms_to_ed ~ "LNW to ED Presentation (mins)",
        sbp_baseline ~ "Baseline SBP (mm Hg)",
        nihss_baseline ~ "Baseline NIHSS",
        gcs_baseline ~ "Baseline GCS",
        htn ~ "Hypertension",
        dm2 ~ "Type II Diabetes",
        stroke ~ "Previous Stroke",
        tobacco ~ "Tobacco Use",
        ich_location ~ "ICH Location",
        ich_volume_baseline ~ "ICH Volume (mL)",
        ivh ~ "IVH",
        study ~ "Study"
      ),
      sort = list(everything() ~ "frequency")
    ) |>
    add_p() |>
    add_overall() |>
    bold_labels()
}

table_2_aggressive_function <- function(x) {
  table_two_vars <- c(
    "neurosurgery_evac",
    "evd",
    "days_mechanical_ventilation",
    "tracheostomy",
    "ich_laterality",
    "comfort_care_binary",
    "early_wlst",
    "dnr_binary"
  )

  x |>
    select(all_of(table_two_vars)) |>
    tbl_summary(
      by = ich_laterality,
      missing = "no",
      label = list(
        neurosurgery_evac ~ "Neurosurgical Intervention",
        evd ~ "Ventriculostomy",
        days_mechanical_ventilation ~ "Mechanical Ventilation Days",
        tracheostomy ~ "Tracheostomy",
        comfort_care_binary ~ "Withdrawal of Life-Sustaining Therapy",
        early_wlst ~ "Early Withdrawal of Life-Sustaining Therapy (< 72 hours)",
        dnr_binary ~ "DNR Order"
      )
    ) |>
    add_overall() |>
    bold_labels()
}
