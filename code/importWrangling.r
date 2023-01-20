# loading libraries----

library(tidyverse)
library(tableone)
library(data.table)
library(haven) # Importing SAS Files
library(janitor)
library(lubridate)
library(readxl)
library(Hmisc)

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
    F07Q04DAY = trachDate,
    F07Q06DAY = evdDate,
    F07Q07 = evdDays,
    F07Q09DAY = nsgyEvacDate,
    F16Q07 = euroVAS90,
    EQ_INDEX = eqIndex90,
    F16Q02 = euroMobility90,
    F16Q03 = euroSelfCare90,
    F16Q04 = euroUsual90,
    F16Q05 = euroPain90,
    F16Q06 = euroAnxiety90,
    F21Q07DAY = comfortCareDate,
    F21Q09DAY = dnrDate,
    F11Q02 = icuLOS,
    F11Q01DAY = hospitalLOS
  )
)
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
  default = "NA"
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
  CTCRQ05 == 7, "Pons",
  CTCRQ05 == 8, "Cerebellum"
))]

# ICH Laterality
atachCT[, ichLaterality := factor(fcase(
  CTCRQ05 == 1, "Right",
  CTCRQ05 == 2, "Right",
  CTCRQ05 == 3, "Right",
  CTCRQ05 == 4, "Left",
  CTCRQ05 == 5, "Left",
  CTCRQ05 == 6, "Left",
  default = "NA"
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

atachWhole[, evd := factor(fcase(
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
  F21Q06 == 0, "No"
))]

### outcome variables-----

#### mRs-----

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

#### euroQOL-----
levels(atachWhole$euroMobility90) <- c("No Problems", "Some Problems", "Confined to Bed")

levels(atachWhole$euroSelfCare90) <- c("No Problems", "Some Problems", "Unable to Wash or Dress Myself")

levels(atachWhole$euroUsual90) <- c("No Problems", "Some Problems", "Unable to Perform My Usual Activities")

levels(atachWhole$euroPain90) <- c("No Pain/Discomfort", "Some Moderate Pain/Discomfort", "Extreme Pain/Discomfort")

levels(atachWhole$euroAnxiety90) <- c("Not Anxious/Depressed", "Moderately Anxious/Depressed", "Extremely Anxious/Depressed")

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
  default = "NA"
))]



## filtering only variables need in final analysis-----

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

# erich-----

## import raw data -----

erichWhole <- as.data.table(read_sas("./data/ERICH/erich_case_100217.sas7bdat"))

## renaming variables-----

renameFunc(
  erichWhole,
  .q(
    ID = id,
    gender = sex,
    Race = race,
    Ethnicity = ethnicity,
    RANKIN_FU1 = mrs90
    mrs180 = RANKIN_FU2,
    mrs365 = RANKIN_FU3,
    euroVAS90 = HLTHSTAT_FU1,
    euroVAS180 = HLTHSTAT_FU2,
    euroVAS365 = HLTHSTAT_FU3,
    barthelFeeding90 = FEED_FU1,
    barthelBathing90 = BATHE_FU1,
    barthelGrooming90 = GROOM_FU1,
    barthelDressing90 = DRESS_FU1,
    barthelBowels90 = BOWELS_FU1,
    barthelBladder90 = BLADDER_FU1,
    barthelToilet90 = TOILET_FU1,
    barthelTransfer90 = TRANSFER_FU1,
    barthelMobility90 = MOBLEVEL_FU1,
    barthelStairs90 = STAIRS_FU1,
    barthelTotal90 = BART_TOT_FU1,
    barthelFeeding180 = FEED_FU2,
    barthelBathing180 = BATHE_FU2,
    barthelGrooming180 = GROOM_FU2,
    barthelDressing180 = DRESS_FU2,
    barthelBowels180 = BOWELS_FU2,
    barthelBladder180 = BLADDER_FU2,
    barthelToilet180 = TOILET_FU2,
    barthelTransfer180 = TRANSFER_FU2,
    barthelMobility180 = MOBLEVEL_FU2,
    barthelStairs180 = STAIRS_FU2,
    barthelTotal180 = BART_TOT_FU2,
    barthelFeeding365 = FEED_FU3,
    barthelBathing365 = BATHE_FU3,
    barthelGrooming365 = GROOM_FU3,
    barthelDressing365 = DRESS_FU3,
    barthelBowels365 = BOWELS_FU3,
    barthelBladder365 = BLADDER_FU3,
    barthelToilet365 = TOILET_FU3,
    barthelTransfer365 = TRANSFER_FU3,
    barthelMobility365 = MOBLEVEL_FU3,
    barthelStairs365 = STAIRS_FU3,
    barthelTotal365 = BART_TOT_FU3
  )
)