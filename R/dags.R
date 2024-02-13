aggressive_dag_function <- function(x) {
  aggressiveness_node_details <- tribble(
    ~name, ~label, ~x, ~y,
    "aggressive_care", "Aggressive Care", 5, 1,
    "ich_laterality", "Hemispheric Laterality", 1, 1,
    "age", "Age", 3.75, 2.5,
    "ich_location", "ICH Location", 2.75, 1.75,
    "ich_volume", "ICH Volume", 1.25, 4,
    "ivh", "IVH", 1, 3,
    "gcs_baseline", "Admission GCS", 4.5, 3.75,
    "stroke", "Previous Stroke", 1, 1,
    "hypertension", "Hypertension", 1, 1,
    "amyloid", "Amyloid Angiopathy", 1, 1
  )

  aggressiveness_node_labels <- aggressiveness_node_details$label
  names(aggressiveness_node_labels) <- aggressiveness_node_details$name

  dagify(
    aggressive_care ~ ich_laterality + ivh + ich_volume + gcs_baseline + ich_location + age + stroke + amyloid,
    ivh ~ ich_location,
    gcs_baseline ~ ich_volume + age + ivh,
    ich_location ~ amyloid + hypertension,
    amyloid ~ age,
    exposure = "ich_laterality",
    outcome = "aggressive_care",
    latent = "amyloid",
    labels = aggressiveness_node_labels
  )
}

outcome_dag_function <- function(x) {
  outcome_node_details <- tribble(
    ~name, ~label, ~x, ~y,
    "functional_outcome", "Functional Outcome", 5, 1,
    "ich_laterality", "Hemispheric Laterality", 1, 1,
    "age", "Age", 3.75, 2.5,
    "ich_location", "ICH Location", 2.75, 1.75,
    "ich_volume", "ICH Volume", 1.25, 4,
    "ivh", "IVH", 1, 3,
    "gcs_baseline", "Admission GCS", 4.5, 3.75,
    "wlst", "Early WLST", 3.75, 2.25,
    "neurosurgery", "Neurosurgery", 2, 1.5
  )

  outcome_node_labels <- outcome_node_details$label
  names(outcome_node_labels) <- outcome_node_details$name

  dagify(
    functional_outcome ~ ich_laterality + age + ich_location + ich_volume + ivh + gcs_baseline + wlst + neurosurgery,
    neurosurgery ~ ich_laterality + age + ich_location + gcs_baseline,
    wlst ~ age + ich_location + gcs_baseline + ich_volume + ivh,
    gcs_baseline ~ age + ich_location + ich_volume + ivh,
    ich_location ~ age,
    ivh ~ ich_location,
    exposure = "ich_laterality",
    outcome = "functional_outcome",
    coords = outcome_node_details,
    labels = outcome_node_labels
  )
}
