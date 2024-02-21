select_variables <- function(x) {
  x |>
    select(
      study,
      age,
      sex,
      race,
      ethnicity,
      time_symptoms_to_ed,
      sbp_baseline,
      nihss_baseline,
      gcs_baseline,
      htn,
      hld,
      dm2,
      stroke,
      afib,
      cad,
      tobacco,
      ich_laterality,
      ich_location,
      ich_volume_baseline,
      ivh,
      ivh_volume_baseline,
      neurosurgery_evac,
      neurosurgery_evac_day,
      evd,
      evd_day,
      days_EVD,
      days_mechanical_ventilation,
      tracheostomy,
      peg,
      dnr_binary,
      dnr_day,
      dni_binary,
      dni_day,
      comfort_care_binary,
      comfort_care_day,
      early_wlst,
      mrs_90,
      mrs_90_01,
      mrs_90_02,
      mrs_90_03,
      mrs_90_04,
      hospital_los
    )
}

filter_variables <- function(x) {
  x |>
    filter(ich_location == "Basal Ganglia" | ich_location == "Thalamus" | ich_location == "Lobar") |>
    drop_na(ich_laterality)
}

 