library(naniar)
library(dplyr)
library(ggplot2)

tar_load(ich_aggressive)
tar_load(theme_ich)

variables <- c(
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
    all_of(starts_with("mrs")),
    all_of(starts_with("euro")),
    -all_of(ends_with("180")),
    -all_of(ends_with("365"))
  )

naniar::gg_miss_var(data, show_pct = TRUE) +
  labs(title = "Percent Missing by Variable") +
  theme_minimal(base_size = 16)

naniar::gg_miss_upset(data, text.scale = 2)

shadow_data <- bind_shadow(data, only_miss = TRUE)

ggplot(
  data = shadow_data,
  aes(
    x = ich_volume_baseline
  )
) +
  geom_density() +
  facet_wrap(~mrs_90_NA, ncol = 1) +
  theme_minimal()

ggplot(shadow_data, aes(x = ich_location)) +
  geom_bar(position = "fill") +
  facet_wrap(~mrs_90_NA) +
  labs(y = "Proportion", title = "Missingness by Location") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data |>
  group_by(study) |>
  miss_var_summary()

gg_miss_fct(x = data, fct = study)

# 1. IVH (Categorical) - Do IVH patients drop out more?
p1 <- ggplot(shadow_data, aes(x = ivh, fill = mrs_90_NA)) +
  geom_bar(position = "fill") + # "fill" shows proportions (0 to 100%)
  labs(y = "Proportion", title = "Missingness by IVH Status") +
  scale_fill_manual(values = c("!NA" = "gray30", "NA" = "firebrick")) +
  theme_minimal()

# 2. Location (Categorical) - Do specific locations drop out more?
p2 <- ggplot(shadow_data, aes(x = ich_location, fill = mrs_90_NA)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", title = "Missingness by Location") +
  scale_fill_manual(values = c("!NA" = "gray30", "NA" = "firebrick")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. GCS (Ordinal) - Is the median GCS lower for missing cases?
p3 <- ggplot(shadow_data, aes(x = mrs_90_NA, y = gcs_baseline)) +
  geom_boxplot() +
  labs(title = "GCS Distribution by Missingness") +
  theme_minimal()

# Display all together
library(patchwork)
p1 + p2 + p3

missing <- data |>
  group_by(ich_volume_baseline) |>
  miss_var_summary() |>
  filter(variable == "mrs_90")

head(missing)
