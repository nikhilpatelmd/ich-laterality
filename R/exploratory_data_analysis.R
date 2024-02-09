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
      )
    ) |>
    add_p() |>
    add_overall() |>
    bold_labels()
}
