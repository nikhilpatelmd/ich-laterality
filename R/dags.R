f_neurosurgery_dag <- function(x) {
  dag <- dagify(
    neurosurgery ~ ich_laterality + ivh + ich_volume + gcs_baseline + ich_location + age + stroke + amyloid,
    ivh ~ ich_location,
    gcs_baseline ~ ich_volume + age + ivh + ich_location,
    ich_location ~ amyloid + hypertension,
    amyloid ~ age,
    hypertension ~ age,
    stroke ~ hypertension + age + amyloid,
    exposure = "ich_laterality",
    outcome = "neurosurgery",
    latent = "amyloid",
    coords = list(
      x = c(
        neurosurgery = 5,
        ich_laterality = 5,
        age = 1,
        ich_location = 3,
        ich_volume = 3.5,
        ivh = 3.25,
        gcs_baseline = 3,
        stroke = 2,
        hypertension = 2,
        amyloid = 2
      ),
      y = c(
        neurosurgery = 5,
        ich_laterality = 0,
        age = 3.75,
        ich_location = 3.75,
        ich_volume = 5.5,
        ivh = 2,
        gcs_baseline = 8,
        stroke = 5,
        hypertension = 2,
        amyloid = 7
      )
      ),
      labels = c(
        neurosurgery = "Neurosurgical Intervention",
        ich_laterality = "Hemispheric Laterality",
        age = "Age",
        ich_location = "ICH Location",
        ich_volume = "ICH Volume",
        ivh = "IVH",
        gcs_baseline = "Admission GCS",
        stroke = "Previous Stroke",
        hypertension = "Hypertension",
        amyloid = "Cerebral Amyloid Angiopathy"
      )
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
    "amyloid", "Amyloid Angiopathy", 2, 7,
    "time", "Time from Symptoms to ED Presentation", 5, 7
  )

  node_labels <- node_details$label
  names(node_labels) <- node_details$name

  dagify(
    functional_outcomes ~ ich_laterality + neurosurgery + wlst + ivh + ich_volume + gcs_baseline + ich_location + age + rehab + stroke + amyloid + hypertension + time,
    rehab ~ ich_laterality + age + ich_location + ich_volume + gcs_baseline + stroke + amyloid + hypertension + time,
    ivh ~ ich_location + ich_volume,
    gcs_baseline ~ ich_volume + age + ivh + ich_location + time,
    ich_location ~ hypertension + amyloid,
    amyloid ~ age,
    hypertension ~ age,
    stroke ~ hypertension + age,
    neurosurgery ~ ich_laterality + ich_location + age + gcs_baseline + amyloid + stroke + time,
    wlst ~ ich_laterality + ich_location + age + gcs_baseline + amyloid + stroke,
    ich_laterality ~ time,
    coords = node_details,
    exposure = "ich_laterality",
    outcome = "functional_outcomes",
    latent = c("rehab", "amyloid"),
    labels = node_labels
  )
}
