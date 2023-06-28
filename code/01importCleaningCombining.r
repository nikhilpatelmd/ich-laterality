# loading libraries----

library(tidyverse)
library(dtplyr)
library(tableone)
library(data.table)
library(haven) # Importing SAS Files
library(janitor)
library(lubridate)
library(readxl)
library(Hmisc)
library(stringr)
library(qs)
getRs("reptools.r") # Loads reptools.r from Github

# loading raw data----

# variables needed from each dataset-----
# demographics: age, gender, site of enrollment
# pmh: hypertension, diabetes, stroke, ICH, CAD, CKD
# meds: anticoagulation, antiplatelet, lipids, bp
# presentation features: symptom onset, SBP/DBP,GCS, NIHSS
# presentation features: intubated
# imaging: ICH volume, location, IVH, midline shift?
# processes: EVD, neurosurgery, trach, PEG, WLST
# outcomes: mRS,


# atach-2----

## import raw data----

# Create function to paste together string of FORMxx.txt
atachImportFunc <- function(y) { # nolint: object_name_linter.
  filepath <- paste0("./data/ATACH2/FORM", y, ".txt")
  dataset <- read.table(filepath, sep = "\t", header = TRUE) # nolint
}

## import data files that are tidy-structured
atachImportVector <- c("00", "01", "02", "03", "04", "05", "07", "08", "11", "16", "17", "21", "33") # nolint # nolint

atachList <- lapply(atachImportVector, FUN = atachImportFunc) # nolint

atachWhole <- as.data.table(reduce(atachList, full_join, by = "SUBJECT_ID")) # nolint

atachWhole[, study := "ATACH2"]

## renaming variables----
renameFunc <- function(x, n) setnames(x, names(n), n)

renameFunc(
  atachWhole,
  .q(
    SUBJECT_ID = id,
    F33Q02 = age,
    F00Q02MIN = symptomOnset,
    F02Q03MIN = arrivalTimeInitial,
    F02Q05MIN = arrivalTimeStrokeCenter,
    F05Q07A = sbpInitial,
    F05Q07B = dbpInitial,
    F33Q04 = gcsBaseline,
    F07Q02 = daysVentilated,
    F07Q03DAY = extubationDay,
    F07Q04DAY = trachDay,
    F07Q06DAY = evdDay,
    F07Q07 = evdDays,
    F07Q09DAY = nsgyEvacDay,
    F16Q07 = euroVAS90,
    EQ_INDEX = eqIndex90,
    F16Q02 = euroMobility90,
    F16Q03 = euroSelfCare90,
    F16Q04 = euroUsual90,
    F16Q05 = euroPain90,
    F16Q06 = euroAnxiety90,
    F21Q07DAY = comfortCareDay,
    F21Q09DAY = dnrDay,
    F11Q02 = icuLOS,
    F11Q01DAY = hospitalLOS,
    F21Q01 = studyEnd,
    F21Q03DAY = studyEndDate
  )
)

as.numeric(atachWhole$comfortCareDay)

## categorizing and recoding Variables----

### demographics----
atachWhole[, race := factor(fcase(
  F01Q03M1 == 1, "American Indian or Alaskan Native",
  F01Q03M2 == 1, "Asian",
  F01Q03M3 == 1, "Black or African-American",
  F01Q03M5 == 1, "White",
  F01Q03M6 == 1, "Other/Not Reported",
  F01Q03M7 == 1, "Other/Not Reported"
))]

atachWhole[, ethnicity := factor(fcase(
  F01Q02 == 1, "Hispanic or Latino",
  F01Q02 == 2, "Not Hispanic or Latino",
  default = "Unknown/Not Reported"
))]

atachWhole[, sex := factor(fcase(
  F01Q01 == 1, "Male",
  F01Q01 == 2, "Female"
))]

atachWhole[, enrollingCountry := factor(fcase(
  COUNTRY == 1, "United States",
  COUNTRY == 2, "Japan",
  COUNTRY == 3, "China",
  COUNTRY == 4, "South Korea",
  COUNTRY == 5, "Taiwan",
  COUNTRY == 6, "Germany",
  default = NA
))]

### past medical history-----

# Having trouble using this function
atachPMHFunc <- function(x, n) {
  x <- deparse(substitute(x))
  n <- deparse(substitute(n))
  atachWhole[, x := factor(fcase(
    n == 0, "No",
    n == 1, "Yes",
    n == 98, "NA"
  ))]
}

atachWhole[, stroke := factor(fcase(
  F03Q01 == 0, "No",
  F03Q01 == 1, "Yes",
  F03Q01 == 98, "NA"
))]

atachWhole[, chf := factor(fcase(
  F03Q03 == 0, "No",
  F03Q03 == 1, "Yes",
  F03Q03 == 98, "NA"
))]

atachWhole[, afib := factor(fcase(
  F03Q04 == 0, "No",
  F03Q04 == 1, "Yes",
  F03Q04 == 98, "NA"
))]

atachWhole[, htn := factor(fcase(
  F03Q07 == 0, "No",
  F03Q07 == 1, "Yes",
  F03Q07 == 98, "NA"
))]

atachWhole[, pvd := factor(fcase(
  F03Q08 == 0, "No",
  F03Q08 == 1, "Yes",
  F03Q08 == 98, "NA"
))]

atachWhole[, hld := factor(fcase(
  F03Q09 == 0, "No",
  F03Q09 == 1, "Yes",
  F03Q09 == 98, "NA"
))]

atachWhole[, dm2 := factor(fcase(
  F03Q12 == 0, "No",
  F03Q12 == 1, "Yes",
  F03Q12 == 98, "NA"
))]

atachWhole[, tobacco := factor(fcase(
  F03Q12 == 1, "Yes",
  F03Q12 == 2, "Yes",
  F03Q12 == 3, "No",
  F03Q12 == 3, "NA"
))]

atachWhole[, cad := factor(fcase(
  F03Q05 == 0, "No",
  F03Q05 == 1, "Yes",
  F03Q06 == 0, "No",
  F03Q06 == 1, "Yes",
  F03Q05 == 98, "NA",
  F03Q06 == 98, "NA"
))]

### medications-----

atachWhole[, htnMeds := factor(fcase(
  F04Q01 == 0, "No",
  F04Q01 == 1, "Yes"
))]

atachWhole[, diabetesMeds := factor(fcase(
  F04Q03 == 0, "No",
  F04Q03 == 1, "Yes"
))]

### presentation features-----

#### time from symptom onset to ED presentation | everything is relative to randomization but making it relative to symptom onset
atachWhole[, symptomsToED := round(abs(arrivalTimeStrokeCenter - symptomOnset), digits = 2)]

# mechanical ventilation

atachWhole[, mechVentilation := factor(fcase(
  F07Q01 == 1, "Yes",
  F07Q01 == 0, "No"
))]

#### NIHSS-----
atachNIHSS <- as.data.table(atachImportFunc("10"))
atachNIHSS <- setnames(atachNIHSS, "SUBJECT_ID", "id")

# Get baseline and 24-hour NIHSS
setkey(atachNIHSS, F10Q24MIN)
atachNIHSS[, nihssBaseline := F10Q22[1], by = id] # baseline
atachNIHSS[, nihhssHour24 := F10Q22[.N], by = id] # 24-hour

# Extracting only columns that are needed in final merge
atachNIHSS <- atachNIHSS[, .(id, nihssBaseline, nihhssHour24)]
atachNIHSS <- unique(atachNIHSS) # removing duplicate NIHSS rows

# Merge data into bigger dataset
atachWhole <- atachNIHSS[atachWhole, on = .(id)]

### imaging data-----

atachCT <- as.data.table(read.table("./data/ATACH2/CTCR.txt", sep = "\t", header = TRUE)) # nolint: line_length_linter.

atachCT <- setnames(atachCT, "SUBJECT_ID", "id")

# Sort out which CT scan was baseline and at 24-hours by taking the highest and lowest value of the CT timing # nolint: line_length_linter.

setkey(atachCT, CTCRMIN)

atachCT[, baselineICHVolume := CTCRQ12[1] / 1000, by = id] # baseline
atachCT[, baselineIVHVolume := CTCRQ13[1] / 1000, by = id]
atachCT[, baselinePHEVolume := CTCRQ11[1] / 1000, by = id]
atachCT[, baselineMidlineShift := CTCRQ14[1], by = id]

atachCT[, hour24ICHVolume := CTCRQ12[.N] / 1000, by = id] # 24-hours
atachCT[, hour24IVHVolume := CTCRQ13[.N] / 1000, by = id]
atachCT[, hour24PHEVolume := CTCRQ11[.N] / 1000, by = id]
atachCT[, hour24MidlineShift := CTCRQ14[.N], by = id]

# Remove 24-hour post rows as don't need them anymore
atachCT <- atachCT[CTCRMIN < 0, ]

# ICH Location
atachCT[, ichLocation := factor(fcase(
  CTCRQ05 == 1, "Thalamus",
  CTCRQ05 == 2, "Basal Ganglia",
  CTCRQ05 == 3, "Lobar",
  CTCRQ05 == 4, "Thalamus",
  CTCRQ05 == 5, "Basal Ganglia",
  CTCRQ05 == 6, "Lobar",
  CTCRQ05 == 7, "Brainstem",
  CTCRQ05 == 8, "Cerebellum",
  default = NA
))]

# ICH Laterality
atachCT[, ichLaterality := factor(fcase(
  CTCRQ05 == 1, "Right",
  CTCRQ05 == 2, "Right",
  CTCRQ05 == 3, "Right",
  CTCRQ05 == 4, "Left",
  CTCRQ05 == 5, "Left",
  CTCRQ05 == 6, "Left",
  default = NA
))]

# IVH
atachCT[, ivh := factor(fcase(
  CTCRQ07 == 0, "No",
  CTCRQ07 == 1, "Yes",
  CTCRQ07 == 98, "NA"
))]

# Hydrocephalus
atachCT[, hydrocephalus := factor(fcase(
  CTCRQ08 == 0, "No",
  CTCRQ08 == 1, "Yes",
  CTCRQ08 == 98, "NA"
))]

# Extracting only columns that are needed in final merge
atachCT <- atachCT[, .(id, baselineICHVolume, baselineIVHVolume, baselineMidlineShift, baselinePHEVolume, hour24ICHVolume, hour24IVHVolume, hour24MidlineShift, hour24PHEVolume, ichLaterality, ichLocation, ivh, hydrocephalus)]

# Merge CT data into bigger dataset
atachWhole <- atachCT[atachWhole, on = .(id)]

### process variables-----

# EVD and neurosurgery
atachWhole[, evd := factor(fcase(
  F07Q05 == 1, "Yes",
  F07Q05 == 0, "No"
))]

atachWhole[, nsgyEvac := factor(fcase(
  F07Q08 == 1, "Yes",
  F07Q08 == 0, "No"
))]

# DNR and WLST status
atachWhole[, dnr := factor(fcase(
  F21Q08 == 1, "Yes",
  F21Q08 == 0, "No"
))]

atachWhole[, comfortCare := factor(fcase(
  F21Q06 == 1, "Yes",
  F21Q06 == 0, "No",
  default = NA
))]

# trachesotomy
atachWhole[, tracheostomy := factor(fcase(
  is.na(trachDay), "No",
  !is.na(trachDay), "Yes",
  default = NA
))]

### outcome variables-----

#### mRS-----

atachMRS <- as.data.table(atachImportFunc("09"))
atachMRS <- setnames(atachMRS, "SUBJECT_ID", "id")

# Extracting both 30 and 90 day mRS
setkey(atachMRS, F09ZFORMDATEDAY)
atachMRS[, mrs30 := F09Q01[1], by = id]
atachMRS[, mrs90 := F09Q01[.N], by = id]

# Extracting only columns that are needed in final merge
atachMRS <- atachMRS[, .(id, mrs30, mrs90)]
atachMRS <- unique(atachMRS) # removing duplicate NIHSS rows

# Merge data into bigger dataset
atachWhole <- atachMRS[atachWhole, on = .(id)]

# Coding mrs 6 as mrsModified

atachWhole <- atachWhole |>
  mutate(patientDied = ifelse(studyEnd == 4, "Yes", "No")) |>
  mutate(mrs30Modified = case_when(
    patientDied == "No" ~ mrs30,
    patientDied == "Yes" & studyEndDate < 30 ~ 6,
    TRUE ~ mrs30
  )) |>
  mutate(mrs90Modified = case_when(
    patientDied == "No" ~ mrs90,
    patientDied == "Yes" & studyEndDate < 90 ~ 6,
    TRUE ~ mrs90
  )) |>
  select(-c(mrs30, mrs90)) |>
  rename(mrs30 = mrs30Modified) |>
  rename(mrs90 = mrs90Modified)

#### euroQOL-----

atachWhole[, euroMobility90 := factor(fcase(
  euroMobility90 == "1", "No Problems",
  euroMobility90 == "2", "Some Problems",
  euroMobility90 == "3", "Confined to Bed",
  default = NA
))]

atachWhole[, euroSelfCare90 := factor(fcase(
  euroSelfCare90 == "1", "No Problems",
  euroSelfCare90 == "2", "Some Problems",
  euroSelfCare90 == "3", "Unable to Wash or Dress Myself",
  default = NA
))]

atachWhole[, euroUsual90 := factor(fcase(
  euroUsual90 == "1", "No Problems",
  euroUsual90 == "2", "Some Problems",
  euroUsual90 == "3", "Unable to Perform My Usual Activities",
  default = NA
))]

atachWhole[, euroPain90 := factor(fcase(
  euroPain90 == "1", "No Pain/Discomfort",
  euroPain90 == "2", "Some Moderate Pain/Discomfort",
  euroPain90 == "3", "Extreme Pain/Discomfort",
  default = NA
))]

atachWhole[, euroAnxiety90 := factor(fcase(
  euroAnxiety90 == "1", "Not Anxious/Depressed",
  euroAnxiety90 == "2", "Moderately Anxious/Depressed",
  euroAnxiety90 == "3", "Extremely Anxious/Depressed",
  default = NA
))]

#### hospital discharge disposition

atachWhole[, hospDischargeDisp := factor(fcase(
  F11Q03 == 1, "Home",
  F11Q03 == 2, "Acute Rehab",
  F11Q03 == 3, "Subacute Rehab",
  F11Q03 == 4, "LTACH",
  F11Q03 == 5, "SNF",
  F11Q03 == 6, "Assisted Living",
  F11Q03 == 7, "Nursing Home Care",
  F11Q03 == 8, "Dead",
  default = NA
))]

## filtering only variables need in final analysis------
atachWhole <- atachWhole[, grep("^F0", colnames(atachWhole)) := NULL]
atachWhole <- atachWhole[, grep("^F1", colnames(atachWhole)) := NULL]
atachWhole <- atachWhole[, grep("^F21", colnames(atachWhole)) := NULL]
atachWhole <- atachWhole[, grep("^F33", colnames(atachWhole)) := NULL]
atachWhole <- atachWhole[, grep("^PRIM", colnames(atachWhole)) := NULL]
atachWhole <- atachWhole[, grep("^SEC", colnames(atachWhole)) := NULL]
atachWhole <- atachWhole[, grep("^PRIM", colnames(atachWhole)) := NULL]
atachWhole <- atachWhole[, grep("^TREAT", colnames(atachWhole)) := NULL]
atachWhole <- atachWhole[, grep("^SITE", colnames(atachWhole)) := NULL]
atachWhole <- atachWhole[, grep("^RANDO", colnames(atachWhole)) := NULL]
atachWhole <- atachWhole[, grep("^COUN", colnames(atachWhole)) := NULL]
atachWhole <- atachWhole[, .q(symptomOnset) := NULL]

atachVariableList <- names(atachWhole[, ])

# erich-----

## import raw data -----

erichWhole <- as.data.table(read_sas("./data/ERICH/erich_case_100217.sas7bdat"))

erichWhole <- erichWhole[, .(ID, age, gender, Race, Ethnicity, D6, D7, D7A, D21, D21A, D22, D25, PHY4, PHY6, PHY6A, PHY9, 1510, 1511, 1512, ED7, ED8, ED36, ED37, ED38, ED39, ED44, ED47_SYS, ED47_DIAS, SH1, SH8, SH9, SH10, SMK1A, MHX1, MHX3, MHX3A, MHX6, MHX8, MHX9, MHX11, MHX12, MHX13, MHX13A, MHX14, MHX16, MHX18, MHX20, MHX21, MHX22, MHX27C, MHX28, SA1, SA4, SA5A, SA6, SE1, SE2, SE3, SE15, LAB5, LAB6, IT5D, detail, ICH_Loc_CT1, ICH_Vol_CT1, IVH_Vol_CT1, ICH_Vol_CT2, ICH_Vol_CT3, ICH_Vol_CT4, IVH_Present, IT5D_DATE, IT5D_TIME, IT5H, IT5H_DATE, IT5H_TIME, IT5I, IT5I_DATE, IT5I_TIME, IT5F, IT5F_SPEC, IT5F_DATE, IT5F_TIME, IT7, IT8, IT8A, IT8B, IT8A_OLD, IT13, IT13A, IT24, IT24A_DATE, IT24A_TIME, IT24A_UNITS, IT29, IT29A, IT29B, IT21, IT22, IT25, IT23, IT28, CC16, CC16_DATE, CC16_TIME, CC17, CC17_DATE, CC17_TIME, CC18, CC18_DATE, CC18_TIME, CX15, CX27, CX30, O3, O3A, O5, O20, AU8, AU8_OTH, RANKIN_FU1, RANKIN_FU2, RANKIN_FU3, MOBILITY_FU1, MOBILITY_FU2, MOBILITY_FU3, SELFCARE_FU1, SELFCARE_FU2, SELFCARE_FU3, USUALACT_FU1, USUALACT_FU2, USUALACT_FU3, PAIN_FU1, PAIN_FU2, PAIN_FU3, ANXIETY_FU1, ANXIETY_FU2, ANXIETY_FU3, HLTHSTAT_FU1, HLTHSTAT_FU2, HLTHSTAT_FU3, FEED_FU1, FEED_FU2, FEED_FU3, BATHE_FU1, BATHE_FU2, BATHE_FU3, GROOM_FU1, GROOM_FU2, GROOM_FU3, DRESS_FU1, DRESS_FU2, DRESS_FU3, BOWELS_FU1, BOWELS_FU2, BOWELS_FU3, BLADDER_FU1, BLADDER_FU2, BLADDER_FU3, TOILET_FU1, TOILET_FU2, TOILET_FU3, TRANSFER_FU1, TRANSFER_FU2, TRANSFER_FU3, MOBLEVEL_FU1, MOBLEVEL_FU2, MOBLEVEL_FU3, STAIRS_FU1, STAIRS_FU2, STAIRS_FU3, BART_TOT_FU1, BART_TOT_FU2, BART_TOT_FU3, EMS33, ED44, CURR_LOC_FU1, CURR_LOC_FU2, CURR_LOC_FU3, IT2, O1, O2_DATE, O2_TIME, O3, O3_OTH, DOD)]

erichWhole[, enrollingCountry := "United States"]
erichWhole[, study := "ERICH"]

## renaming variables-----

renameFunc(
  erichWhole,
  .q(
    ID = id,
    gender = sex,
    Race = race,
    Ethnicity = ethnicity,
    ED47_SYS = sbpInitial,
    ED47_DIAS = dbpInitial,
    ED39 = gcsBaseline,
    RANKIN_FU1 = mrs90,
    RANKIN_FU2 = mrs180,
    RANKIN_FU3 = mrs365,
    HLTHSTAT_FU1 = euroVAS90,
    HLTHSTAT_FU2 = euroVAS180,
    HLTHSTAT_FU3 = euroVAS365,
    FEED_FU1 = barthelFeeding90,
    BATHE_FU1 = barthelBathing90,
    GROOM_FU1 = barthelGrooming90,
    DRESS_FU1 = barthelDressing90,
    BOWELS_FU1 = barthelBowels90,
    BLADDER_FU1 = barthelBladder90,
    TOILET_FU1 = barthelToilet90,
    TRANSFER_FU1 = barthelTransfer90,
    MOBLEVEL_FU1 = barthelMobility90,
    STAIRS_FU1 = barthelStairs90,
    BART_TOT_FU1 = barthelTotal90,
    FEED_FU2 = barthelFeeding180,
    BATHE_FU2 = barthelBathing180,
    GROOM_FU2 = barthelGrooming180,
    DRESS_FU2 = barthelDressing180,
    BOWELS_FU2 = barthelBowels180,
    BLADDER_FU2 = barthelBladder180,
    TOILET_FU2 = barthelToilet180,
    TRANSFER_FU2 = barthelTransfer180,
    MOBLEVEL_FU2 = barthelMobility180,
    STAIRS_FU2 = barthelStairs180,
    BART_TOT_FU2 = barthelTotal180,
    FEED_FU3 = barthelFeeding365,
    BATHE_FU3 = barthelBathing365,
    GROOM_FU3 = barthelGrooming365,
    DRESS_FU3 = barthelDressing365,
    BOWELS_FU3 = barthelBowels365,
    BLADDER_FU3 = barthelBladder365,
    TOILET_FU3 = barthelToilet365,
    TRANSFER_FU3 = barthelTransfer365,
    MOBLEVEL_FU3 = barthelMobility365,
    STAIRS_FU3 = barthelStairs365,
    BART_TOT_FU3 = barthelTotal365,
    ICH_Vol_CT1 = baselineICHVolume,
    IVH_Vol_CT1 = baselineIVHVolume,
    IT8A_OLD = evdDay,
    MOBILITY_FU1 = euroMobility90,
    MOBILITY_FU2 = euroMobility180,
    MOBILITY_FU3 = euroMobility365,
    SELFCARE_FU1 = euroSelfCare90,
    SELFCARE_FU2 = euroSelfCare180,
    SELFCARE_FU3 = euroSelfCare365,
    USUALACT_FU1 = euroUsual90,
    USUALACT_FU2 = euroUsual180,
    USUALACT_FU3 = euroUsual365,
    PAIN_FU1 = euroPain90,
    PAIN_FU2 = euroPain180,
    PAIN_FU3 = euroPain365,
    ANXIETY_FU1 = euroAnxiety90,
    ANXIETY_FU2 = euroAnxiety180,
    ANXIETY_FU3 = euroAnxiety365,
    CURR_LOC_FU1 = location90,
    CURR_LOC_FU2 = location180,
    CURR_LOC_FU3 = location365
  )
)

## categorizing and recoding Variables----

### demographics----

erichWhole[, sex := c("Male", "Female")[match(sex, c("M", "F"))]]

erichWhole[, race := c("White", "Black or African-American", "Asian", "Native Hawaiian/Pacific Islander", "American Indian or Alaskan Native", "Other/Not Reported", "Other/Not Reported")[match(race, c("W", "B", "A", "N", "I", "O", "U"))]]

erichWhole[, ethnicity := c("Hispanic or Latino", "Not Hispanic or Latino", "Not Hispanic or Latino")[match(ethnicity, c("H", "N", "U"))]]

### past medical history-----

erichWhole[, stroke := c("No", "Yes", "NA")[match(SH1, c("2", "1", "8"))]]

erichWhole[, cad := c("No", "Yes", "NA")[match(MHX8, c("2", "1", "8"))]]

erichWhole[, chf := c("No", "Yes", "NA")[match(MHX13, c("2", "1", "8"))]]

erichWhole[, afib := c("No", "Yes", "NA")[match(MHX11, c("2", "1", "8"))]]

erichWhole[, pvd := c("No", "Yes", "NA")[match(MHX27C, c("2", "1", "8"))]]

erichWhole[, hld := c("No", "Yes", "NA")[match(MHX6, c("2", "1", "8"))]]

erichWhole[, dm2 := factor(fcase(
  MHX3 == "2", "No",
  MHX3 == "8", "NA",
  MHX3A == "1", "No",
  MHX3A == "2", "Yes",
  MHX3A == "8", "NA",
  default = NA
))]

erichWhole[, tobacco := factor(fcase(
  SMK1A == "2", "No",
  SMK1A == "1", "Yes",
  SMK1A == "5", "NA",
  SMK1A == "8", "NA",
  default = NA
))]

### medications-----

### presentation features-----

#### parsing dates and times in ERICH

# symptom onset
f <- "%Y-%m-%d %H:%M:%S"
g <- "%H:%M:%S"
h <- "%d/%m/%Y"

erichWhole[, symptomOnsetDateTime := as.POSIXct(paste(erichWhole$SE1, erichWhole$SE2), format = f, tz = "EST")]

# converting estimated symptom onset to actual date-times
erichWhole[, symptomOnsetEstimatedDateTime := as.POSIXct(paste(erichWhole$SE1, fcase(
  SE3 == "1", "06:00:00",
  SE3 == "2", "NA",
  SE3 == "3", "03:00:00",
  SE3 == "4", "09:00:00",
  SE3 == "5", "15:00:00",
  default = NA
)), format = f, tz = "EST")]

# combining symptom onset date-time and symptom onset estimated time
erichWhole[, symptomOnsetCombined := as.POSIXct(ifelse(is.na(symptomOnsetDateTime), symptomOnsetEstimatedDateTime, symptomOnsetDateTime), origin = "1970-01-01")]

# time to ED presentation
erichWhole[, edDateTime := as.POSIXct(paste(ED7, ED8), format = f, tz = "EST")]

erichWhole[, symptomsToED := abs(difftime(edDateTime, symptomOnsetCombined, units = "hours"))]

erichWhole[, symptomsToED := ifelse(symptomsToED == 0, NA, symptomsToED * 60)]

# No NIHSS

# mechanical ventilation
erichWhole[, mechVentilation := factor(fcase(
  EMS33 == "1", "Yes",
  EMS33 == "2", "No",
  ED44 == "1", "Yes",
  ED44 == "2", "No",
  IT7 == "1", "Yes",
  IT7 == "2", "Yes",
  default = NA
))]

### imaging -----
erichWhole[, ichLocation := factor(fcase(
  grepl("Ganglia", detail, ignore.case = TRUE), "Basal Ganglia",
  grepl("Brainstem", detail, ignore.case = TRUE), "Brainstem",
  grepl("Cerebell", detail, ignore.case = TRUE), "Cerebellum",
  grepl("Front", detail, ignore.case = TRUE), "Lobar",
  grepl("Tempor", detail, ignore.case = TRUE), "Lobar",
  grepl("Occi", detail, ignore.case = TRUE), "Lobar",
  grepl("Perivent", detail, ignore.case = TRUE), "Basal Ganglia",
  grepl("Caudate", detail, ignore.case = TRUE), "Basal Ganglia",
  grepl("Putam", detail, ignore.case = TRUE), "Basal Ganglia",
  grepl("Deep", detail, ignore.case = TRUE), "Basal Ganglia",
  grepl("Lobar", detail, ignore.case = TRUE), "Lobar",
  grepl("IVH", detail, ignore.case = TRUE), "Primary IVH",
  grepl("Hemis", detail, ignore.case = TRUE), "Lobar",
  grepl("Thal", detail, ignore.case = TRUE), "Thalamus",
  grepl("Corona", detail, ignore.case = TRUE), "Basal Ganglia",
  default = NA
))]

erichWhole[, ivh := factor(ifelse(baselineIVHVolume == 0.00, "No", "Yes"))]

erichWhole[, ichLaterality := factor(fcase(
  PHY6A == "L", "Left",
  PHY6A == "R", "Right",
  default = NA
))]

### process variables -----

erichWhole[, evd := factor(fcase(
  IT8 == "1", "Yes",
  IT8 == "2", "No",
  default = NA
))]

erichWhole[, nsgyEvac := factor(fcase(
  IT5D == "1", "Yes",
  IT5H == "1", "Yes",
  grepl("crani", IT5F_SPEC, ignore.case = TRUE), "Yes",
  grepl("evac", IT5F_SPEC, ignore.case = TRUE), "Yes",
  default = "No"
))]

# neurosurgery dates

erichWhole[, nsgyEvacDateTime := as.POSIXct(fcase(
  IT5D == "1", paste(IT5D_DATE, IT5D_TIME),
  IT5H == "1", paste(IT5H_DATE, IT5H_TIME),
  nsgyEvac == "Yes", paste(IT5F_DATE, IT5F_TIME)
), format = f, tz = "EST")]

erichWhole[, nsgyEvacHour := abs(difftime(edDateTime, nsgyEvacDateTime, units = "hours"))]

erichWhole[, nsgyEvacDay := as.numeric(ceiling(nsgyEvacHour / 24))]

# trach
erichWhole[, tracheostomy := factor(fcase(
  grepl("trach", IT5F_SPEC, ignore.case = TRUE), "Yes",
  default = "No"
))]

# PEG
erichWhole[, peg := factor(fcase(
  grepl("gastro", IT5F_SPEC, ignore.case = TRUE), "Yes",
  default = "No"
))]

# WLST
erichWhole[, dnr := c("No", "Yes", "NA")[match(CC16, c("2", "1", "8"))]]

erichWhole[, dni := c("No", "Yes", "NA")[match(CC17, c("2", "1", "8"))]]

erichWhole[, comfortCare := c("No", "Yes", "NA")[match(CC18, c("2", "1", "8"))]]

erichWhole[, dnrDateTime := as.POSIXct(paste(CC16_DATE, CC16_TIME), format = f, tz = "EST")]

erichWhole[, dniDateTime := as.POSIXct(paste(CC17_DATE, CC17_TIME), format = f, tz = "EST")]

erichWhole[, comfortDateTime := as.POSIXct(paste(CC18_DATE, CC18_TIME), format = f, tz = "EST")]

erichWhole[, dnrHour := abs(difftime(edDateTime, dnrDateTime, units = "hours"))]

erichWhole[, dnrDay := as.numeric(ceiling(dnrHour / 24))]

erichWhole[, dniHour := abs(difftime(edDateTime, dniDateTime, units = "hours"))]

erichWhole[, dniDay := ceiling(dniHour / 24)]

erichWhole[, comfortCareHour := abs(difftime(edDateTime, comfortDateTime, units = "hours"))]

erichWhole[, comfortCareDay := as.numeric(ceiling(comfortCareHour / 24))]

### outcome variables -----

# adding mRS 6 to data
erichWhole[, deathDate := ymd(DOD)]

erichWhole[, deathDay := abs(difftime(as.Date(edDateTime), deathDate, units = "days"))]

erichWhole[deathDay < 90, mrs90 := 6]
erichWhole$mrs90 <- na_if(erichWhole$mrs90, 8)

erichWhole[deathDay < 180, mrs180 := 6]
erichWhole[deathDay < 365, mrs365 := 6]

# EuroQOL
erichWhole[, euroMobility90 := factor(fcase(
  euroMobility90 == "1", "No Problems",
  euroMobility90 == "2", "Some Problems",
  euroMobility90 == "3", "Confined to Bed",
  default = NA
))]

erichWhole[, euroSelfCare90 := factor(fcase(
  euroSelfCare90 == "1", "No Problems",
  euroSelfCare90 == "2", "Some Problems",
  euroSelfCare90 == "3", "Unable to Wash or Dress Myself",
  default = NA
))]

erichWhole[, euroUsual90 := factor(fcase(
  euroUsual90 == "1", "No Problems",
  euroUsual90 == "2", "Some Problems",
  euroUsual90 == "3", "Unable to Perform My Usual Activities",
  default = NA
))]

erichWhole[, euroPain90 := factor(fcase(
  euroPain90 == "1", "No Pain/Discomfort",
  euroPain90 == "2", "Some Moderate Pain/Discomfort",
  euroPain90 == "3", "Extreme Pain/Discomfort",
  default = NA
))]

erichWhole[, euroAnxiety90 := factor(fcase(
  euroAnxiety90 == "1", "Not Anxious/Depressed",
  euroAnxiety90 == "2", "Moderately Anxious/Depressed",
  euroAnxiety90 == "3", "Extremely Anxious/Depressed",
  default = NA
))]

erichWhole[, euroMobility180 := factor(fcase(
  euroMobility180 == "1", "No Problems",
  euroMobility180 == "2", "Some Problems",
  euroMobility180 == "3", "Confined to Bed",
  default = NA
))]

erichWhole[, euroSelfCare180 := factor(fcase(
  euroSelfCare180 == "1", "No Problems",
  euroSelfCare180 == "2", "Some Problems",
  euroSelfCare180 == "3", "Unable to Wash or Dress Myself",
  default = NA
))]

erichWhole[, euroUsual180 := factor(fcase(
  euroUsual180 == "1", "No Problems",
  euroUsual180 == "2", "Some Problems",
  euroUsual180 == "3", "Unable to Perform My Usual Activities",
  default = NA
))]

erichWhole[, euroPain180 := factor(fcase(
  euroPain180 == "1", "No Pain/Discomfort",
  euroPain180 == "2", "Some Moderate Pain/Discomfort",
  euroPain180 == "3", "Extreme Pain/Discomfort",
  default = NA
))]

erichWhole[, euroAnxiety180 := factor(fcase(
  euroAnxiety180 == "1", "Not Anxious/Depressed",
  euroAnxiety180 == "2", "Moderately Anxious/Depressed",
  euroAnxiety180 == "3", "Extremely Anxious/Depressed",
  default = NA
))]

erichWhole[, euroMobility365 := factor(fcase(
  euroMobility365 == "1", "No Problems",
  euroMobility365 == "2", "Some Problems",
  euroMobility365 == "3", "Confined to Bed",
  default = NA
))]

erichWhole[, euroSelfCare365 := factor(fcase(
  euroSelfCare365 == "1", "No Problems",
  euroSelfCare365 == "2", "Some Problems",
  euroSelfCare365 == "3", "Unable to Wash or Dress Myself",
  default = NA
))]

erichWhole[, euroUsual365 := factor(fcase(
  euroUsual365 == "1", "No Problems",
  euroUsual365 == "2", "Some Problems",
  euroUsual365 == "3", "Unable to Perform My Usual Activities",
  default = NA
))]

erichWhole[, euroPain365 := factor(fcase(
  euroPain365 == "1", "No Pain/Discomfort",
  euroPain365 == "2", "Some Moderate Pain/Discomfort",
  euroPain365 == "3", "Extreme Pain/Discomfort",
  default = NA
))]

erichWhole[, euroAnxiety365 := factor(fcase(
  euroAnxiety365 == "1", "Not Anxious/Depressed",
  euroAnxiety365 == "2", "Moderately Anxious/Depressed",
  euroAnxiety365 == "3", "Extremely Anxious/Depressed",
  default = NA
))]

# location during follow-up

erichWhole[, location90 := factor(fcase(
  location90 == "1", "Home",
  location90 == "2", "Rehab Facility",
  location90 == "3", "Long-Term Care Facility",
  location90 == "4", "Hospital",
  location90 == "5", "Other",
  mrs90 == "6", "Dead",
  default = NA
))]

erichWhole[, location180 := factor(fcase(
  location180 == "1", "Home",
  location180 == "2", "Rehab Facility",
  location180 == "3", "Long-Term Care Facility",
  location180 == "4", "Hospital",
  location180 == "5", "Other",
  mrs180 == "6", "Dead",
  default = NA
))]

erichWhole[, location365 := factor(fcase(
  location365 == "1", "Home",
  location365 == "2", "Rehab Facility",
  location365 == "3", "Long-Term Care Facility",
  location365 == "4", "Hospital",
  location365 == "5", "Other",
  mrs365 == "6", "Dead",
  default = NA
))]

### filtering only variables need in final analysis------
erichWhole <- erichWhole[, grep("^O", colnames(erichWhole)) := NULL]
erichWhole <- erichWhole[, grep("^I", colnames(erichWhole)) := NULL]
erichWhole <- erichWhole[, grep("^E", colnames(erichWhole)) := NULL]
erichWhole <- erichWhole[, grep("^C", colnames(erichWhole)) := NULL]
erichWhole <- erichWhole[, grep("^A", colnames(erichWhole)) := NULL]
erichWhole <- erichWhole[, grep("^deta", colnames(erichWhole)) := NULL]
erichWhole <- erichWhole[, grep("^LA", colnames(erichWhole)) := NULL]
erichWhole <- erichWhole[, grep("^S", colnames(erichWhole)) := NULL]
erichWhole <- erichWhole[, grep("^M", colnames(erichWhole)) := NULL]
erichWhole <- erichWhole[, grep("^V", colnames(erichWhole)) := NULL]
erichWhole <- erichWhole[, grep("^P", colnames(erichWhole)) := NULL]
erichWhole <- erichWhole[, grep("^D", colnames(erichWhole)) := NULL]

### exporting variable list-----
erichVariableList <- names(erichWhole[, ])

# clear iii -----

## import raw data -----
clear3Demographics <- fread("./data/CLEAR3/demographics.csv")
clear3Outcomes <- fread("./data/CLEAR3/outcomes.csv")

clear3Whole <- clear3Demographics[clear3Outcomes, on = .(new_patient_number)]

clear3Whole <- cleanup.import(clear3Whole)

## renaming variables-----
renameFunc(
  clear3Whole,
  .q(
    studyname = study,
    new_patient_number = id,
    age_at_registration = age,
    gender = sex,
    hispanic_latino = ethnicity,
    tobacco_use = tobacco,
    cocaine_use = cocaine,
    anticoagulated_at_registration = anticoagulated,
    adjudicated_ich_location = ichLocation,
    hemisphere = ichLaterality,
    er_present_systolic = sbpInitial,
    er_present_diastolic = dbpInitial,
    er_present_gcs_total = gcsBaseline,
    er_present_nihss_total = nihssBaseline,
    dct_ich_volume_rc = baselineICHVolume,
    dct_ivh_volume_rc = baselineIVHVolume,
    ictus_to_presentation = symptomsToED,
    glasgow_rankin_30 = mrs30,
    glasgow_rankin_180 = mrs180,
    gose_30 = gose30,
    gose_180 = gose180,
    withdrawal_contribution = comfortCare,
    ictus_to_withdrawal_days = comfortCareDay
  )
)

## categorizing and recoding Variables----

### demographics----
clear3Whole[, race := factor(fcase(
  race == "African-American", "Black or African-American",
  race == "Asian", "Asian",
  race == "Hawaiian or Pacific", "Native Hawaiian/Pacific Islander",
  race == "Indian or Alaskan", "American Indian or Alaskan Native",
  race == "Indian or Alaskan;White", "American Indian or Alaskan Native",
  race == "unknown/missing", "Other/Not Reported",
  race == "White", "White"
))]

clear3Whole[, ethnicity := factor(fcase(
  ethnicity == "0", "Not Hispanic or Latino",
  ethnicity == "1", "Hispanic or Latino",
  default = NA
))]

clear3Whole[, sex := factor(fcase(
  sex == "male", "Male",
  sex == "female", "Female",
  default = NA
))]

### past medical history-----
clear3Whole[, tobacco := factor(fcase(
  tobacco == "1", "Yes",
  tobacco == "NA", "No"
))]

### presentation features -----
clear3Whole[, symptomsToED := round(symptomsToED * 24 * 60, digits = 0)]

### imaging -----
clear3Whole[, ichLocation := factor(fcase(
  ichLocation == "Caudate", "Basal Ganglia",
  ichLocation == "Frontal", "Lobar",
  ichLocation == "Globus Pallidus", "Basal Ganglia",
  ichLocation == "Occipital", "Lobar",
  ichLocation == "Parietal", "Lobar",
  ichLocation == "Probable Primary IVH", "Primary IVH",
  ichLocation == "Putamen", "Basal Ganglia",
  ichLocation == "Temporal", "Lobar",
  ichLocation == "Thalamus", "Thalamus",
  default = NA
))]

clear3Whole[, ivh := "Yes"]

clear3Whole[, ichLaterality := factor(fcase(
  ichLaterality == "left", "Left",
  ichLaterality == "right", "Right",
  default = NA
))]

### process variables -----
clear3Whole[, comfortCareDay := as.numeric(round(comfortCareDay, digits = 0))]

clear3Whole[, comfortCare := ifelse(comfortCare == "on", "Yes", "No")]

clear3Whole[, evd := "Yes"]

clear3Whole[, nsgyEvac := "No"]

### filtering only variables need in final analysis------
clear3Whole <- clear3Whole[, .(study, id, age, sex, race, ethnicity, tobacco, cocaine, anticoagulated, ichLaterality, ichLocation, sbpInitial, dbpInitial, gcsBaseline, nihssBaseline, baselineIVHVolume, baselineICHVolume, symptomsToED, mrs30, mrs180, comfortCare, comfortCareDay, ivh)]

### exporting variable list-----
clear3VariableList <- names(clear3Whole[, ])

# mistie ii-----

## import raw data -----
mistie2Demographics <- data.table(fread("./data/MISTIE2/demographics.csv"))
mistie2Imaging <- data.table(fread("./data/MISTIE2/imaging.csv"))
mistie2Outcomes <- data.table(fread("./data/MISTIE2/outcomes.csv"))
mistie2publication1 <- data.table(fread("./data/MISTIE2/publicationResults.csv"))
mistie2publication2 <- data.table(fread("./data/MISTIE2/publicationResults2.csv"))

# converting outcomes from long to wide format
mistie2OutcomesWide <- dcast(mistie2Outcomes, id ~ Follow_up_Visit, value.var = "rankin_score")

mistie2Whole <- Merge(mistie2Demographics, mistie2Imaging, mistie2OutcomesWide, mistie2publication1, mistie2publication2, id = ~id)

mistie2Whole <- as.data.table(mistie2Whole)

mistie2Whole[, study := "MISTIE II"]

## renaming variables-----
renameFunc(
  mistie2Whole,
  .q(
    symptom_onset_age = age,
    Gender = sex,
    Hispanic_Latino = ethnicity,
    Tobacco_Use = tobacco,
    Cocaine_Use = cocaine,
    Hypertension = htn,
    Hyperlipidemia = hld,
    Diabetes = dm2,
    Other_CV = cad,
    clot_location_rc = ichLocation,
    subject_has_ivh = ivh,
    hemisphere = ichLaterality,
    ER_Present_Systolic = sbpInitial,
    ER_Present_Diastolic = dbpInitial,
    ER_Present_GCS_Total = gcsBaseline,
    enrollment_nihss_total.x = nihssBaseline,
    Diagnostic_CT_ICH_Volume = baselineICHVolume,
    Diagnostic_CT_IVH_Volume = baselineIVHVolume,
    Ictus_to_Randomization = symptomsToED,
    "30" = mrs30,
    "90" = mrs90,
    "180" = mrs180,
    "270" = mrs270,
    "365" = mrs365
  )
)

## categorizing and recoding Variables----

## demographics----

mistie2Whole[, race := factor(fcase(
  Race == "African American not Hispanic", "Black or American-American",
  Race == "Asian or Pacific Islander", "Asian",
  Race == "Caucasian not Hispanic", "White",
  Race == "Hispanic", "Other/Not Reported",
  Race == "Other or unknown", "Other/Not Reported",
  default = NA
))]

mistie2Whole[, ethnicity := factor(fcase(
  Race == "African American not Hispanic", "Not Hispanic or Latino",
  Race == "Hispanic", "Hispanic or Latino",
  Race == "Asian or Pacific Islander", "Not Hispanic or Latino",
  Race == "Caucasian not Hispanic", "Not Hispanic or Latino",
  Race == "Other or unknown", "Not Hispanic or Latino",
  default = NA
))]

## imaging----

mistie2Whole[, ichLocation := factor(fcase(
  ichLocation == "Globus Palidus", "Basal Ganglia",
  ichLocation == "Lobar", "Lobar",
  ichLocation == "Putamen", "Basal Ganglia",
  ichLocation == "Thalamus", "Thalamus",
  default = NA
))]

mistie2Whole$ichLaterality <- na_if(mistie2Whole$ichLaterality, "Discrepancy")

mistie2Whole[, symptomsToED := round(symptomsToED * 60, digits = 0)]

## process variables------

mistie2Whole[, nsgyEvac := factor(fcase(
  mistie2Demographics$Group_Assigned == "Medical", "No",
  mistie2Demographics$Group_Assigned == "Surgical", "Yes"
))]

mistie2Whole[, evd := as.factor("No")]

## filtering only variables need in final analysis------

mistie2Whole <- mistie2Whole[, .(id, study, gcsBaseline, sbpInitial, dbpInitial, sex, race, ethnicity, htn, hld, dm2, tobacco, cocaine, ivh, baselineICHVolume, baselineIVHVolume, symptomsToED, nsgyEvac, evd, mrs30, mrs90, mrs180, mrs270, mrs365, age, nihssBaseline, ichLocation, ichLaterality)]

## exporting variable list-----
mistie2VariableList <- names(mistie2Whole[, ])

# mistie iii------

## import raw data -----
mistie3Whole <- data.table(fread("./data/MISTIE3/data.csv"))
mistie3Whole[, study := "MISTIE III"]

## renaming variables-----

renameFunc(
  mistie3Whole,
  .q(
    treatment_group = nsgyEvac,
    age_at_consent = age,
    patientid_ninds = id,
    er_present_systolic = sbpInitial,
    er_present_diastolic = dbpInitial,
    gcs_total = gcsBaseline,
    nihss_total = nihssBaseline,
    diagct_ich_volume = baselineICHVolume,
    diagct_ivh_volume = baselineIVHVolume,
    ich_hemisphere_adj = ichLaterality,
    glasgow_rankin_30 = mrs30,
    glasgow_rankin_180 = mrs180,
    glasgow_rankin_365 = mrs365,
    barthel_index_365 = barthelTotal365,
    eq_vas_365 = euroVAS365,
    ictus_to_diagct_hours = symptomsToED,
    eq5d_mobility_365 = euroMobility365,
    eq5d_selfcare_365 = euroSelfCare365,
    eq5d_activities_365 = euroUsual365,
    eq5d_pain_365 = euroPain365,
    eq5d_anxiety_365 = euroAnxiety365,
    location_d30 = location30,
    location_d180 = location180,
    location_d365 = location365
  )
)

mistie3Whole$nsgy
## categorizing and recoding Variables----

### demographics----
mistie3Whole[, race := factor(fcase(
  race_ninds == "African-American", "Black or African-American",
  race_ninds == "Asian Hawaiian or Pacific", "Asian",
  race_ninds == "Indian or Alaskan", "American Indian or Alaskan Native",
  race_ninds == "More than one race", "Other/Not Reported",
  race_ninds == "White", "White",
  default = NA
))]

mistie3Whole[, ethnicity := factor(fcase(
  hispanic == "0", "Not Hispanic or Latino",
  hispanic == "1", "Hispanic or Latino"
))]

mistie3Whole[, sex := factor(fcase(
  male_gender == "0", "Female",
  male_gender == "1", "Male"
))]

mistie3Whole[, tobacco := factor(fcase(
  current_smoker == "0", "No",
  current_smoker == "1", "Yes"
))]

mistie3Whole[, dm2 := factor(fcase(
  diabetes_pmh == "0", "No",
  diabetes_pmh == "1", "Yes"
))]

mistie3Whole[, htn := factor(fcase(
  htn_pmh == "0", "No",
  htn_pmh == "1", "Yes"
))]

mistie3Whole[, cad := factor(fcase(
  cvd_pmh == "0", "No",
  cvd_pmh == "1", "Yes"
))]

mistie3Whole[, hld := factor(fcase(
  hyperlipidemia == "0", "No",
  hyperlipidemia == "1", "Yes"
))]

### presentation features -----
mistie3Whole[, symptomsToED := round(symptomsToED * 60, digits = 0)]

### imaging -----
mistie3Whole[, ichLocation := factor(fcase(
  ich_location_specify == "BG", "Basal Ganglia",
  ich_location_specify == "Frontal", "Lobar",
  ich_location_specify == "Occipital", "Lobar",
  ich_location_specify == "Parietal", "Lobar",
  ich_location_specify == "Temporal", "Lobar",
  ich_location_specify == "Thalamus", "Thalamus",
  default = NA
))]

mistie3Whole[, ivh := factor(ifelse(baselineIVHVolume == 0.00, "No", "Yes"))]

### process variables -----

mistie3Whole[, comfortCare := factor(fcase(
  withdrawal_contribution == "0", "No",
  withdrawal_contribution == "1", "Yes",
  default = NA
))]

mistie3Whole[, nsgyEvac := factor(fcase(
  nsgyEvac == "medical", "No",
  nsgyEvac == "surgical", "Yes"
))]

mistie3Whole[, evd := as.factor("No")]

# comfort care day

mistie3Whole[, symptomOnset := as.Date(ninds_symptom_onset_date, format = "%m/%d/%Y")]

mistie3Whole[, comfortDateTime := as.Date(mistie3Whole$ninds_withdrawal_date, format = "%m/%d/%Y")]

mistie3Whole[, comfortCareDay := as.numeric(abs(difftime(symptomOnset, comfortDateTime, units = "days")))]

### outcome variables -----

# EuroQOL

mistie3Whole[, euroMobility365 := factor(fcase(
  euroMobility365 == "1", "No Problems",
  euroMobility365 == "2", "Some Problems",
  euroMobility365 == "3", "Confined to Bed",
  default = NA
))]

mistie3Whole[, euroSelfCare365 := factor(fcase(
  euroSelfCare365 == "1", "No Problems",
  euroSelfCare365 == "2", "Some Problems",
  euroSelfCare365 == "3", "Unable to Wash or Dress Myself",
  default = NA
))]

mistie3Whole[, euroUsual365 := factor(fcase(
  euroUsual365 == "1", "No Problems",
  euroUsual365 == "2", "Some Problems",
  euroUsual365 == "3", "Unable to Perform My Usual Activities",
  default = NA
))]

mistie3Whole[, euroPain365 := factor(fcase(
  euroPain365 == "1", "No Pain/Discomfort",
  euroPain365 == "2", "Some Moderate Pain/Discomfort",
  euroPain365 == "3", "Extreme Pain/Discomfort",
  default = NA
))]

mistie3Whole[, euroAnxiety365 := factor(fcase(
  euroAnxiety365 == "1", "Not Anxious/Depressed",
  euroAnxiety365 == "2", "Moderately Anxious/Depressed",
  euroAnxiety365 == "3", "Extremely Anxious/Depressed",
  default = NA
))]


# location after discharge
mistie3Whole[, location30 := factor(fcase(
  location30 == "Acute - Hospital", "Hospital",
  location30 == "Dead", "Dead",
  location30 == "Home", "Home",
  location30 == "LTCF", "Long-Term Care Facility",
  location30 == "LTFU", "Long-Term Care Facility",
  location30 == "Rehab", "Rehab Facility",
  location30 == "Withdrawn", "Other",
  default = NA
))]

mistie3Whole[, location180 := factor(fcase(
  location180 == "Acute - Hospital", "Hospital",
  location180 == "Dead", "Dead",
  location180 == "Home", "Home",
  location180 == "LTCF", "Long-Term Care Facility",
  location180 == "LTFU", "Long-Term Care Facility",
  location180 == "Rehab", "Rehab Facility",
  location180 == "Withdrawn", "Other",
  default = NA
))]

mistie3Whole[, location365 := factor(fcase(
  location365 == "Acute - Hospital", "Hospital",
  location365 == "Dead", "Dead",
  location365 == "Home", "Home",
  location365 == "LTCF", "Long-Term Care Facility",
  location365 == "LTFU", "Long-Term Care Facility",
  location365 == "Rehab", "Rehab Facility",
  location365 == "Withdrawn", "Other",
  default = NA
))]

## filtering only variables need in final analysis------

mistie3Whole <- mistie3Whole[, .(id, age, tobacco, hld, cocaine, dm2, htn, cad, sbpInitial, dbpInitial, gcsBaseline, nihssBaseline, baselineICHVolume, baselineIVHVolume, mrs30, mrs180, mrs365, barthelTotal365, euroVAS365, euroMobility365, euroSelfCare365, euroUsual365, euroPain365, euroAnxiety365, location30, location180, location365, symptomsToED, ichLaterality, ichLocation, study, race, ethnicity, sex, ivh, comfortCare, comfortCareDay, nsgyEvac, evd)]


## exporting variable list-----
mistie3VariableList <- names(mistie3Whole[, ])

# combing all datasets-----
a <- list(atachWhole, erichWhole, clear3Whole, mistie2Whole, mistie3Whole)

nindsCombined <- rbindlist(a, fill = TRUE)

## cleaning up master dataset-----
setcolorder(nindsCombined, .q(id, study, age, sex, race, ethnicity, gcsBaseline, ichLaterality, ichLocation))

nindsCombined[, .q(arrivalTimeInitial, arrivalTimeStrokeCenter, enrollingCountry, mechVentilation, symptomOnsetDateTime, symptomOnsetEstimatedDateTime, symptomOnsetCombined, edDateTime, nsgyEvacDateTime, nsgyEvacHour, peg, deathDate, anticoagulated, comfortDateTime, comfortCareHour, dniDateTime, dnrDateTime, dnrHour, dniHour, dniDay) := NULL]

nindsCombined[, baselineICHVolume := ceiling(baselineICHVolume)]
nindsCombined[, baselineIVHVolume := ceiling(baselineIVHVolume)]
nindsCombined[, baselinePHEVolume := ceiling(baselinePHEVolume)]
nindsCombined[, hour24ICHVolume := ceiling(hour24ICHVolume)]
nindsCombined[, hour24IVHVolume := ceiling(hour24IVHVolume)]
nindsCombined[, hour24PHEVolume := ceiling(hour24PHEVolume)]


nindsCombined <- nindsCombined %>%
  mutate(across(.q(stroke, chf, afib, htn, pvd, cad, hld, tobacco, dnr, comfortCare), na_if, "NA"))

## Computing ICH Score-----

nindsCombined[, ichScoreVolume := fcase(
  baselineICHVolume >= 30, 1,
  baselineICHVolume < 30, 0,
  default = 0
)]

nindsCombined[, ichScoreGCS := fcase(
  gcsBaseline >= 13, 0,
  gcsBaseline <= 4, 2,
  default = 1
)]

nindsCombined[, ichScoreIVH := fcase(
  ivh == "Yes", 1,
  ivh == "No", 0,
  default = 0
)]

nindsCombined[, ichScoreAge := fcase(
  age >= 80, 1,
  age < 80, 0
)]

nindsCombined[, ichScoreLocation := fcase(
  ichLocation == "Cerebellum", 1,
  ichLocation == "Brainstem", 1,
  default = 0
)]

nindsCombined[, ichScoreTotal := ichScoreVolume + ichScoreGCS + ichScoreIVH + ichScoreAge + ichScoreLocation]

## Filtering out NAs in ICH Location and ICH Laterality, and filtering out brainstem, cerebellar, and primary IVH

nindsCombined <- nindsCombined[ichLaterality != "NA", ]

nindsCombined <- nindsCombined |>
  filter(ichLocation != "Cerebellum") |>
  filter(ichLocation != "Brainstem") |>
  filter(ichLocation != "Primary IVH") |>
  droplevels()

table(nindsCombined$ichLaterality)

## mRS as factors and dropping unused levels

nindsCombined$mrs30 <- as.factor(nindsCombined$mrs30)
nindsCombined$mrs90 <- as.factor(nindsCombined$mrs90)
nindsCombined$mrs180 <- as.factor(nindsCombined$mrs180)
nindsCombined$mrs270 <- as.factor(nindsCombined$mrs270)
nindsCombined$mrs365 <- as.factor(nindsCombined$mrs365)

## Re-ordered EuroQOL

nindsCombined$euroMobility90 <- factor(nindsCombined$euroMobility90, levels = c("No Problems", "Some Problems", "Confined to Bed"))

nindsCombined$euroMobility180 <- factor(nindsCombined$euroMobility180, levels = c("No Problems", "Some Problems", "Confined to Bed"))

nindsCombined$euroMobility365 <- factor(nindsCombined$euroMobility365, levels = c("No Problems", "Some Problems", "Confined to Bed"))

nindsCombined$euroSelfCare90 <- factor(nindsCombined$euroSelfCare90, levels = c("No Problems", "Some Problems", "Unable to Wash or Dress Myself"))

nindsCombined$euroSelfCare180 <- factor(nindsCombined$euroSelfCare180, levels = c("No Problems", "Some Problems", "Unable to Wash or Dress Myself"))

nindsCombined$euroSelfCare365 <- factor(nindsCombined$euroSelfCare365, levels = c("No Problems", "Some Problems", "Unable to Wash or Dress Myself"))

nindsCombined$euroUsual90 <- factor(nindsCombined$euroUsual90, levels = c("No Problems", "Some Problems", "Unable to Perform My Usual Activities"))

nindsCombined$euroUsual180 <- factor(nindsCombined$euroUsual180, levels = c("No Problems", "Some Problems", "Unable to Perform My Usual Activities"))

nindsCombined$euroUsual365 <- factor(nindsCombined$euroUsual365, levels = c("No Problems", "Some Problems", "Unable to Perform My Usual Activities"))

nindsCombined$euroPain90 <- factor(nindsCombined$euroPain90, levels = c("No Pain/Discomfort", "Some Moderate Pain/Discomfort", "Extreme Pain/Discomfort"))

nindsCombined$euroPain180 <- factor(nindsCombined$euroPain180, levels = c("No Pain/Discomfort", "Some Moderate Pain/Discomfort", "Extreme Pain/Discomfort"))

nindsCombined$euroPain365 <- factor(nindsCombined$euroPain365, levels = c("No Pain/Discomfort", "Some Moderate Pain/Discomfort", "Extreme Pain/Discomfort"))

nindsCombined$euroAnxiety90 <- factor(nindsCombined$euroAnxiety90, levels = c("Not Anxious/Depressed", "Moderately Anxious/Depressed", "Extremely Anxious/Depressed"))

nindsCombined$euroAnxiety180 <- factor(nindsCombined$euroAnxiety180, levels = c("Not Anxious/Depressed", "Moderately Anxious/Depressed", "Extremely Anxious/Depressed"))

nindsCombined$euroAnxiety365 <- factor(nindsCombined$euroAnxiety365, levels = c("Not Anxious/Depressed", "Moderately Anxious/Depressed", "Extremely Anxious/Depressed"))

## Coding Early WLST (as <= 3 days)

nindsCombined[, earlyWLST := factor(fcase(
  comfortCareDay <= 3, "Yes",
  comfortCareDay > 3, "No",
  default = NA
))]

## Exporting Data-----

qsave(nindsCombined, "./data/nindsCombined.qs")

## Visualizing Data-----

table(nindsCombined$study, nindsCombined$mrs90)
