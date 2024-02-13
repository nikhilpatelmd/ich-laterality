aggressive_dag_function <- function(x) {
  node_details <- tribble(
    ~name, ~label, ~x, ~y,
    "aggressive_care", "Aggressive Care", 5, 5,
    "ich_laterality", "Hemispheric Laterality", 3.25, 0,
    "age", "Age", 1, 3.75,
    "ich_location", "ICH Location", 3, 3.75,
    "ich_volume", "ICH Volume", 3.5, 5.5,
    "ivh", "IVH", 3.25, 2,
    "gcs_baseline", "Admission GCS", 3, 8,
    "stroke", "Previous Stroke", 2, 5,
    "hypertension", "Hypertension", 2, 2,
    "amyloid", "Amyloid Angiopathy", 2, 7,
  )

  node_labels <- node_details$label
  names(node_labels) <- node_details$name

  dagify(
    aggressive_care ~ ich_laterality + ivh + ich_volume + gcs_baseline + ich_location + age + stroke + amyloid,
    ivh ~ ich_location,
    gcs_baseline ~ ich_volume + age + ivh + ich_location,
    ich_location ~ amyloid + hypertension,
    amyloid ~ age,
    hypertension ~ age,
    stroke ~ hypertension + age + amyloid,
    coords = node_details,
    exposure = "ich_laterality",
    outcome = "aggressive_care",
    latent = "amyloid",
    labels = node_labels
  )
}

outcomes_dag_function <- function(x) {
  node_details <- tribble(
    ~name, ~label, ~x, ~y,
    "functional_outcomes", "Functional Outcomes", 8, 5,
    "ich_laterality", "Hemispheric Laterality", 3, 0,
    "age", "Age", 1, 3.75,
    "ich_location", "ICH Location", 2.75, 3.75,
    "ich_volume", "ICH Volume", 3, 6,
    "ivh", "IVH", 3, 2,
    "gcs_baseline", "Admission GCS", 3, 8,
    "neurosurgery", "Neurosurgical Intervention", 6, 4,
    "wlst", "Early WLST", 5, 2,
    "rehab", "Participation in Rehab", 7, 1,
    "stroke", "Previous stroke", 2, 5,
    "hypertension", "Hypertension", 2, 2,
    "amyloid", "Amyloid Angiopathy", 2, 7
  )

  node_labels <- node_details$label
  names(node_labels) <- node_details$name

  dagify(
    functional_outcomes ~ ich_laterality + neurosurgery + wlst + ivh + ich_volume + gcs_baseline + ich_location + age + rehab + stroke + amyloid + hypertension,
    rehab ~ ich_laterality + age + ich_location + ich_volume + gcs_baseline + stroke + amyloid + hypertension,
    ivh ~ ich_location + ich_volume,
    gcs_baseline ~ ich_volume + age + ivh + ich_location,
    ich_location ~ hypertension + amyloid,
    amyloid ~ age,
    hypertension ~ age,
    stroke ~ hypertension + age + amyloid,
    neurosurgery ~ ich_laterality + ich_location + age + gcs_baseline + amyloid + stroke,
    wlst ~ ich_laterality + ich_location + age + gcs_baseline + amyloid + stroke,
    coords = node_details,
    exposure = "ich_laterality",
    outcome = "functional_outcomes",
    latent = c("rehab", "amyloid"),
    labels = node_labels
  )
}
