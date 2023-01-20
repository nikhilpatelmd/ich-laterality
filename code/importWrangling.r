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
    F07Q02 = intubationDays,
    F07Q03DAY = extubationDays,
    F07Q04DAY = trachDate,
    F07Q07 = evdDays,
    F07Q09DAY = nsgyEvacDate,
    F16Q07 = euroVAS90,
    EQ_INDEX = eqIndex90
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

### imaging data-----
atachCT <- as.data.table(read.table("./data/ATACH2/CTCR.txt", sep = "\t", header = TRUE)) # nolint: line_length_linter.
