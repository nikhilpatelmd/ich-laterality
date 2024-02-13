aggressive_dag_function <- function(x) {
  node_details <- tribble(
    ~name, ~label, ~x, ~y,
    "aggressive_care", "Aggressive Care", 4, 5,
    "ich_laterality", "Hemispheric Laterality", 3, 0,
    "age", "Age", 1, 3.25,
    "ich_location", "ICH Location", 3, 4,
    "ich_volume", "ICH Volume", 3, 5.5,
    "ivh", "IVH", 3, 2,
    "gcs_baseline", "Admission GCS", 3, 8,
    "stroke", "Previous Stroke", 2, 5,
    "hypertension", "Hypertension", 2, 2.25,
    "amyloid", "Amyloid Angiopathy", 2, 6.25,
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
