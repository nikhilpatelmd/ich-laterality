library(gtsummary)
library(dplyr)
library(smd)

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
      sort = all_categorical() ~ "frequency",
      digits = starts_with("gcs") ~ 0,
    ) |>
    # --- CHANGED SECTION START ---
    add_stat(
      fns = everything() ~ function(data, variable, ...) {
        # 1. Safely extract x and y
        x <- data[[variable]]
        y <- data$ich_laterality

        # 2. Handle 'difftime' (Time objects) specifically
        if (inherits(x, "difftime")) {
          x <- as.numeric(x, units = "mins")
        }

        # 3. CRITICAL FIX: Remove Missing Data
        # smd() propagates NAs, so we must filter them out before calculation
        valid_idx <- !is.na(x) & !is.na(y)
        x_clean <- x[valid_idx]
        y_clean <- y[valid_idx]

        # 4. Calculate SMD safely using the CLEAN vectors
        smd_res <- tryCatch(
          {
            smd::smd(x = x_clean, g = y_clean)$estimate
          },
          error = function(e) NA_real_
        )

        # 5. Handle NaNs or NAs (e.g., zero variance or empty vectors)
        if (
          is.null(smd_res) ||
            length(smd_res) == 0 ||
            is.nan(smd_res) ||
            is.na(smd_res)
        ) {
          return("NA")
        }

        # 6. Format output
        return(as.character(style_sigfig(smd_res, digits = 2)))
      },
      location = everything() ~ "label"
    ) |>
    modify_header(add_stat_1 ~ "**SMD**") |>
    # --- CHANGED SECTION END ---
    add_overall() |>
    bold_labels() |>
    as_gt()

  return(table_1)
}
