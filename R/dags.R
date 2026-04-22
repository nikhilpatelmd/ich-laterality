# R/dags.R
# Directed acyclic graph (DAG) constructors for the neurosurgery and
# functional-outcome analyses.
#
# Both functions return a dagitty object ready for ggdag plotting and
# adjustment-set derivation. They are called as upstream targets in
# _targets.R (dag_neurosurgery, dag_outcomes) so that downstream figure
# targets automatically invalidate whenever the DAG structure changes.


# ---------------------------------------------------------------------------
# Neurosurgery DAG
# Exposure:  hemispheric laterality
# Outcome:   neurosurgical intervention (evacuation or EVD)
# Estimand:  confounder-adjusted total effect
#            (no mediators are conditioned on)
# ---------------------------------------------------------------------------

f_neurosurgery_dag <- function() {
  dagify(
    # Surgical decision. ICH characteristics, clinical severity, and
    # prior vascular history are all independent determinants.
    neurosurgery ~ ich_laterality +
      ich_volume +
      ich_location +
      ivh +
      gcs_baseline +
      age +
      stroke +
      amyloid,

    # IVH: determined by proximity to ventricles (location) and
    # hematoma size (volume).
    ivh ~ ich_location + ich_volume,

    # Admission GCS reflects direct ICH severity (volume, location)
    # and secondary depression from IVH-related obstruction.
    gcs_baseline ~ ich_volume +
      age +
      ivh +
      ich_location,

    ich_location ~ amyloid + hypertension,
    amyloid ~ age,
    hypertension ~ age,
    stroke ~ hypertension + age + amyloid,

    exposure = "ich_laterality",
    outcome = "neurosurgery",

    # amyloid (CAA) is not directly measured; it is inferred from imaging
    # pattern and age, and not recorded in ERICH or ATACH-2 as a variable.
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


# ---------------------------------------------------------------------------
# Functional Outcomes DAG
# Exposure:  hemispheric laterality
# Outcome:   mRS / EuroQOL at 90 days
# Estimand:  total effect
#            (neurosurgery, wlst, and rehab are mediators — not conditioned on)
# ---------------------------------------------------------------------------

outcomes_dag_function <- function() {
  # Node coordinates defined as a tribble so labels travel with positions.
  node_details <- tribble(
    ~name                 , ~label                       , ~x   , ~y   ,
    "functional_outcomes" , "Functional Outcomes"        , 8    , 5    ,
    "ich_laterality"      , "Hemispheric Laterality"     , 3    , 0    ,
    "age"                 , "Age"                        , 1    , 3.75 ,
    "ich_location"        , "ICH Location"               , 2.75 , 3.75 ,
    "ich_volume"          , "ICH Volume"                 , 3    , 6    ,
    "ivh"                 , "IVH"                        , 3    , 2    ,
    "gcs_baseline"        , "Admission GCS"              , 3    , 8    ,
    "neurosurgery"        , "Neurosurgical Intervention" , 6    , 4    ,
    "wlst"                , "Early WLST"                 , 5    , 2    ,
    "rehab"               , "Participation in Rehab"     , 7    , 1    ,
    "stroke"              , "Previous Stroke"            , 2    , 5    ,
    "hypertension"        , "Hypertension"               , 2    , 2    ,
    "amyloid"             , "Amyloid Angiopathy"         , 2    , 7    ,
    "time"                , "Time: Symptom Onset to ED"  , 5    , 7
  )

  node_labels <- setNames(node_details$label, node_details$name)

  dagify(
    # Functional outcome at 90 days is influenced by the direct biological
    # effect of laterality, by downstream treatment decisions (neurosurgery,
    # wlst, rehab), and by all baseline clinical characteristics.
    functional_outcomes ~ ich_laterality +
      neurosurgery +
      wlst +
      rehab +
      ivh +
      ich_volume +
      gcs_baseline +
      ich_location +
      age +
      stroke +
      amyloid +
      hypertension +
      time,

    # Neurosurgical intervention: same determinants as in the primary DAG,
    # plus time-to-presentation (which affects eligibility windows).
    neurosurgery ~ ich_laterality +
      ich_volume +
      ich_location +
      ivh +
      gcs_baseline +
      age +
      stroke +
      amyloid +
      time,

    # WLST: driven by clinical severity and the laterality of the lesion
    # (left-sided ICH may prompt earlier WLST discussions due to perceived
    # language/cognitive prognosis).
    wlst ~ ich_laterality +
      ich_volume +
      ich_location +
      gcs_baseline +
      age +
      stroke +
      amyloid,

    # Rehab participation: influenced by laterality (functional deficits
    # vary by hemisphere), clinical characteristics, and time to presentation.
    rehab ~ ich_laterality +
      ich_volume +
      ich_location +
      gcs_baseline +
      age +
      stroke +
      amyloid +
      hypertension +
      time,

    # IVH: proximity to ventricles and hematoma size.
    ivh ~ ich_location + ich_volume,

    # Admission GCS reflects direct severity and secondary IVH-related effects.
    gcs_baseline ~ ich_volume +
      age +
      ivh +
      ich_location +
      time,

    ich_location ~ hypertension + amyloid,
    amyloid ~ age,
    hypertension ~ age,
    stroke ~ hypertension + age,

    # Laterality is influenced by time-to-presentation only insofar as the
    # clinical recognition of ICH (and thus enrollment) can differ by
    # hemisphere due to symptom salience (e.g., aphasia prompting faster
    # 911 calls for left-sided ICH).
    ich_laterality ~ time,

    coords = node_details,
    exposure = "ich_laterality",
    outcome = "functional_outcomes",

    # amyloid: not directly measured in these datasets.
    # rehab: rehabilitation participation is not recorded in ERICH or ATACH-2.
    latent = c("amyloid", "rehab"),

    labels = node_labels
  )
}
