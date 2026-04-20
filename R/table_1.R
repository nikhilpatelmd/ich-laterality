table_1_function <- function(x) {
  table_one_vars <- c(
    "age", "sex", "race", "ethnicity", "time_symptoms_to_ed",
    "sbp_baseline", "nihss_baseline", "gcs_baseline", "ich_laterality",
    "htn", "dm2", "stroke", "tobacco", "ich_location",
    "ich_volume_baseline", "ivh", "study"
  )

  table_1 <- x |>
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
      sort = all_categorical() ~ "alphanumeric",
      digits = starts_with("gcs") ~ 0,
    ) |>
    add_overall() |>

    add_stat(
      fns = everything() ~ function(data, variable, ...) {
        x_vec <- data[[variable]]
        y_vec <- data$ich_laterality
        if (inherits(x_vec, "difftime")) x_vec <- as.numeric(x_vec, units = "mins")
        valid_idx <- !is.na(x_vec) & !is.na(y_vec)
        smd_res <- tryCatch({ smd::smd(x = x_vec[valid_idx], g = y_vec[valid_idx])$estimate }, error = function(e) NA_real_)
        if (is.null(smd_res) || is.na(smd_res)) return("NA")
        return(as.character(style_sigfig(smd_res, digits = 2)))
      },
      location = everything() ~ "label"
    ) |>
    
    modify_header(add_stat_1 ~ "**SMD**") |>
    modify_footnote(everything() ~ NA) |>
    bold_labels() |>
    as_gt() |>
    gt::tab_source_note(
      source_note = "Values are n/total n (%) or median (25th–75th percentiles) [no. of available patients in case of missing data]. LNW indicates last known well; ED, emergency department; SBP, systolic blood pressure; NIHSS, National Institutes of Health Stroke Scale; GCS, Glasgow Coma Scale; ICH, intracerebral hemorrhage; IVH, intraventricular hemorrhage; and SMD, standardized mean difference."
    )

  return(table_1)
}