dag_function <- function(x) {
  node_details <- tribble(
    ~name, ~label, ~x, ~y,
    "functional_disability", "Functional Disability", 4, 1,
    "ich_laterality", "Hemispheric Laterality", 1, 1,
    "age", "Age", 2.75, 3,
    "ich_location", "ICH Location", 2, 2.75,
    "ich_volume", "ICH Volume", 1, 2.75,
    "ivh", "IVH", 1.5, 2.25,
    "gcs_baseline", "Admission GCS", 2, 3.25,
    "wlst", "Early WLST", 3.75, 2.25,
    "neurosurgery", "Neurosurgery", 2, 1.5,
  )

  node_labels <- node_details$label
  names(node_labels) <- node_details$name
}
