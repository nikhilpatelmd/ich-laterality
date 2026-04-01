f_neurosurgery_dag <- function(x) {
  dagify(
    # Primary surgical decision node.
    # Midline shift, hydrocephalus, and eloquent-area involvement are all
    # independent determinants of surgical eligibility not fully captured
    # by volume + location alone — hence their inclusion as latent nodes.
    neurosurgery ~ ich_laterality +
      ivh +
      ich_volume +
      gcs_baseline +
      ich_location +
      age +
      stroke +
      amyloid +
      midline_shift +
      hydrocephalus +
      eloquence,

    # Mass effect variables: mediators on the path from ICH characteristics
    # to surgical decisions. Conditioning on these in the primary model
    # would block part of the causal path from volume/location to surgery
    # and is therefore inappropriate. Represented as latent because
    # they are only measured in ATACH-2.
    midline_shift ~ ich_volume + ich_location + ivh,
    hydrocephalus ~ ich_volume + ich_location + ivh,

    # Eloquence captures unmeasured granularity in ICH location (e.g.,
    # proximity to Broca/Wernicke, corticospinal tract involvement) beyond
    # the lobar vs. deep categorization available in our data.
    eloquence ~ ich_location,

    # IVH depends on both proximity to ventricles (location) and
    # hemorrhage size (volume).
    ivh ~ ich_location + ich_volume,

    # Admission GCS is depressed both by the hematoma itself and by
    # downstream mass effect (herniation, obstructive hydrocephalus).
    gcs_baseline ~ ich_volume +
      age +
      ivh +
      ich_location +
      midline_shift +
      hydrocephalus,

    ich_location ~ amyloid + hypertension,
    amyloid ~ age,
    hypertension ~ age,
    stroke ~ hypertension + age + amyloid,

    exposure = "ich_laterality",
    outcome = "neurosurgery",
    latent = c("amyloid", "midline_shift", "hydrocephalus", "eloquence"),

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
        amyloid = 2,
        midline_shift = 4.5,
        hydrocephalus = 4.25,
        eloquence = 4
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
        amyloid = 7,
        midline_shift = 6.5,
        hydrocephalus = 3,
        eloquence = 4.5
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
      amyloid = "Cerebral Amyloid Angiopathy",
      midline_shift = "Midline Shift",
      hydrocephalus = "Hydrocephalus",
      eloquence = "Eloquent Area Involvement"
    )
  )
}


outcomes_dag_function <- function(x) {
  node_details <- tribble(
    ~name                 , ~label                                , ~x   , ~y   ,
    "functional_outcomes" , "Functional Outcomes"                 , 8    , 5    ,
    "ich_laterality"      , "Hemispheric Laterality"              , 3    , 0    ,
    "age"                 , "Age"                                 , 1    , 3.75 ,
    "ich_location"        , "ICH Location"                        , 2.75 , 3.75 ,
    "ich_volume"          , "ICH Volume"                          , 3    , 6    ,
    "ivh"                 , "IVH"                                 , 3    , 2    ,
    "gcs_baseline"        , "Admission GCS"                       , 3    , 8    ,
    "neurosurgery"        , "Neurosurgical Intervention"          , 6    , 4    ,
    "wlst"                , "Early WLST"                          , 5    , 2    ,
    "rehab"               , "Participation in Rehab"              , 7    , 1    ,
    "stroke"              , "Previous Stroke"                     , 2    , 5    ,
    "hypertension"        , "Hypertension"                        , 2    , 2    ,
    "amyloid"             , "Amyloid Angiopathy"                  , 2    , 7    ,
    "time"                , "Time from Symptoms to ED"            , 5    , 7    ,
    # Mass effect mediators — latent in pooled analysis, observed in ATACH-2 only
    "midline_shift"       , "Midline Shift"                       , 4.75 , 6    ,
    "hydrocephalus"       , "Hydrocephalus"                       , 4.5  , 3    ,
    # lang_framing is a *mediator* of ich_laterality → wlst/rehab:
    # dominant-hemisphere injury may lead clinicians to frame prognosis
    # differently (aphasia, language deficits), influencing WLST and rehab
    # referral independent of true biological severity.
    "lang_framing"        , "Language-Related Prognostic Framing" , 4    , 1    ,
    # family_pref is an unmeasured common cause of both WLST and rehab
    # decisions — a confounder of that pair, not caused by laterality.
    "family_pref"         , "Family Preferences"                  , 6.5  , 2
  )

  node_labels <- setNames(node_details$label, node_details$name)

  dagify(
    functional_outcomes ~ ich_laterality +
      neurosurgery +
      wlst +
      ivh +
      ich_volume +
      gcs_baseline +
      ich_location +
      age +
      rehab +
      stroke +
      amyloid +
      hypertension +
      time +
      midline_shift +
      hydrocephalus,

    # Rehab participation: driven partly by laterality (via language framing)
    # and by unmeasured family preferences.
    rehab ~ ich_laterality +
      age +
      ich_location +
      ich_volume +
      gcs_baseline +
      stroke +
      amyloid +
      hypertension +
      time +
      lang_framing +
      family_pref,

    # Mass effect mediators: downstream of ICH characteristics, upstream of
    # surgical and functional outcomes. Conditioning on these in the primary
    # model constitutes over-adjustment.
    midline_shift ~ ich_volume + ich_location + ivh,
    hydrocephalus ~ ich_volume + ich_location + ivh,

    # Admission GCS reflects both direct ICH severity and mass effect.
    gcs_baseline ~ ich_volume +
      age +
      ivh +
      ich_location +
      time +
      midline_shift +
      hydrocephalus,

    ivh ~ ich_location + ich_volume,
    ich_location ~ hypertension + amyloid,
    amyloid ~ age,
    hypertension ~ age,
    stroke ~ hypertension + age,

    # Surgical decision: mass effect variables are independent determinants
    # of surgical eligibility beyond what volume alone captures.
    neurosurgery ~ ich_laterality +
      ich_location +
      age +
      gcs_baseline +
      amyloid +
      stroke +
      time +
      midline_shift +
      hydrocephalus +
      ivh +
      ich_volume,

    # WLST: influenced by language framing (mediator) and family preferences
    # (unmeasured confounder of wlst ↔ rehab).
    wlst ~ ich_laterality +
      ich_location +
      age +
      gcs_baseline +
      amyloid +
      stroke +
      lang_framing +
      family_pref,

    # The key mediator edge: left-hemispheric ICH triggers a different
    # prognostic narrative around language/aphasia, which then shapes
    # WLST and rehab decisions. This means our total-effect estimate of
    # laterality legitimately includes this pathway — we should not
    # condition on lang_framing.
    lang_framing ~ ich_laterality,

    ich_laterality ~ time,

    coords = node_details,
    exposure = "ich_laterality",
    outcome = "functional_outcomes",
    latent = c(
      "rehab",
      "amyloid",
      "midline_shift",
      "hydrocephalus", # only measured in ATACH-2
      "lang_framing",
      "family_pref" # never measured
    ),
    labels = node_labels
  )
}
