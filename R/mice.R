variables <- c(
  "neurosurgery_evac",
  "ich_laterality",
  "ich_location",
  "age",
  "gcs_baseline",
  "ich_volume_baseline",
  "study"
  )

data <- ich_aggressive |>
  select(variables)

mice(ich_aggressive, m = 5, maxit = 35000, nnet.MaxNWts = 40000, print = FALSE)