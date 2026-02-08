f_missing_data_filter <- function(ich_aggressive) {
  library(naniar)
  library(dplyr)
  library(ggplot2)
  library(showtext)
  library(stringr)

  variables <- c(
    "site_id",
    "neurosurgery_evac",
    "evd",
    "days_mechanical_ventilation",
    "dnr_binary",
    "comfort_care_binary",
    "early_wlst",
    "tracheostomy",
    "ich_laterality",
    "ich_location",
    "age",
    "ivh",
    "gcs_baseline",
    "ich_volume_baseline",
    "study",
    "htn",
    "time_symptoms_to_ed"
  )

  data <- ich_aggressive |>
    select(
      all_of(variables),
      starts_with(c("mrs", "euro")),
      -ends_with(c("180", "365")),
      -ends_with(c("01", "02", "03", "04"))
    )

  return(data)
}

f_percent_missing_visual_stratified <- function(data, variable_name) {
  plot_data <- data |>
    mutate(grouping_var = .data[[variable_name]])

  clean_name <- variable_name |>
    str_replace_all("_", " ") |>
    str_to_title() |>
    str_replace_all("Ich", "ICH")

  percent_missing <- naniar::gg_miss_var(
    plot_data,
    facet = grouping_var,
    show_pct = TRUE
  ) +
    labs(title = paste("Missing Data by", clean_name)) +
    theme_minimal(
      base_size = 16,
      base_family = "Liberation Sans"
    )

  return(percent_missing)
}


f_shadow_plots <- function(data, plotting_variable, missing_variable) {
  library(naniar)
  shadow_data <- bind_shadow(data, only_miss = TRUE) |>
    mutate(grouping_variable = .data[[plotting_variable]])

  clean_x_name <- plotting_variable |>
    str_replace_all("_", " ") |>
    str_to_title() |>
    str_replace_all("Ich", "ICH") |>
    str_replace_all("Gcs", "GCS")

  clean_y_name <- missing_variable |>
    str_replace_all("_", " ") |>
    str_to_title() |>
    str_replace_all("Mrs", "mRS") |>
    str_replace_all("Vas", "VAS")

  missing_col_name <- paste0(missing_variable, "_NA")

  ggplot(
    data = shadow_data,
    aes(x = grouping_variable, fill = .data[[missing_col_name]])
  ) +
    geom_density(alpha = 0.5) +
    facet_wrap(vars(.data[[missing_col_name]]), ncol = 1) +
    labs(
      title = glue("Distribution of {clean_x_name}"),
      subtitle = glue("Stratified by missingness in {clean_y_name}"),
      x = clean_x_name,
      y = "Density",
      fill = "Missing?"
    ) +
    theme_minimal(
      base_size = 16,
      base_family = "Liberation Sans"
    ) +
    theme(
      legend.position = "top",
      axis.title.x = element_blank()
    )
}



ich_aggressive |>
  group_by(ich_laterality) |>
  naniar::miss_var_summary() |>
  filter(variable == "mrs_90")