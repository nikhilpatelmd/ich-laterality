
library(tidyverse)
library(tableone)
library(data.table)
library(haven) # Importing SAS Files
library(janitor)
library(lubridate)
library(readxl)
library(Hmisc)
library(kableExtra)

# Create function to paste together string of FORMxx.txt
atach.import <- function(y) {
  dataset <- paste0("atach2",".",y)
  filepath <- paste0("./data/ATACH2/FORM",y,".txt")
  dataset <- read.table(filepath, sep = "\t", header = TRUE)
}

atach00 <- atach.import("00")
atach01 <- atach.import("01")
atach02 <- atach.import("02")
atach03 <- atach.import("03")
atach04 <- atach.import("04")
atach05 <- atach.import("05")
# atach05c <- atach.import("05c") Detailed Blood Pressure Data, Unsure if Needed
atach06 <- atach.import("06")
atach07 <- atach.import("07")
atach08 <- atach.import("08")
atach09 <- atach.import("09")
atach10 <- atach.import("10")
atach11 <- atach.import("11")
atach13 <- atach.import("13")
atach14 <- atach.import("14")
atach15 <- atach.import("15")
atach16 <- atach.import("16")
atach17 <- atach.import("17")
atach18 <- atach.import("18")
atach20 <- atach.import("20")
atach21 <- atach.import("21")
atach23 <- atach.import("23")
atach33 <- atach.import("33")
atachct <- read.table("./data/ATACH2/CTCR.txt", sep = "\t", header = TRUE)

# Merging all tidy data
atach.tidy <- list(atach00, atach01, atach02, atach03, atach04, atach05, atach07, atach08, atach11, atach16, atach17, atach21, atach33)
atach.whole <- reduce(atach.tidy, full_join, by = "SUBJECT_ID")
atach.whole <-atach.whole %>% mutate(study = "ATACH2")

# Importing CT Data
atachct <- atachct %>%
  mutate(ctnumber = case_when(
    CTCRMIN < 0 ~ "baseline",
    CTCRMIN > 0 ~ "24hourpost"
  ))

# Extracting First CT Data
atachctbaseline <- atachct %>% 
  filter(ctnumber == "baseline") %>%
  select(SUBJECT_ID, CTCRMIN, CTCRQ05, CTCRQ07, CTCRQ08, CTCRQ09, CTCRQ12, CTCRQ13, CTCRQ11, CTCRQ14) %>%
  rename(CTCRMIN.baseline = CTCRMIN) %>%
  rename(ich.location.baseline = CTCRQ05) %>%
  rename(ivh.baseline = CTCRQ07) %>%
  rename(hydrocephalus.baseline = CTCRQ08) %>%
  rename(pineal.shift.baseline = CTCRQ09) %>%
  rename(ich.volume.baseline = CTCRQ12) %>%
  rename(ivh.volume.baseline = CTCRQ13) %>%
  rename(phe.volume.baseline = CTCRQ11) %>%
  rename(sp.shift.baseline = CTCRQ14)


atachct24hrpost <- atachct %>% # Extracting Second CT Data
  filter(ctnumber == "24hourpost") %>%
    select(SUBJECT_ID, CTCRMIN, CTCRQ05, CTCRQ07, CTCRQ08, CTCRQ09, CTCRQ12, CTCRQ13, CTCRQ11, CTCRQ14) %>%
  rename(CTCRMIN.24hourpost = CTCRMIN) %>%
  rename(ich.location.24hourpost = CTCRQ05) %>%
  rename(ivh.24hourpost = CTCRQ07) %>%
  rename(hydrocephalus.24hourpost = CTCRQ08) %>%
  rename(pineal.shift.24hourpost = CTCRQ09) %>%
  rename(ich.volume.24hourpost = CTCRQ12) %>%
  rename(ivh.volume.24hourpost = CTCRQ13) %>%
  rename(phe.volume.24hourpost = CTCRQ11) %>%
  rename(sp.shift.24hourpost = CTCRQ14)

atach.whole <- left_join(atach.whole, atachctbaseline, by = "SUBJECT_ID")
atach.whole <- left_join(atach.whole, atachct24hrpost, by = "SUBJECT_ID")


# Importing mRS Data

atachmrs30 <- atach09 %>%
  filter(mrsday == 30) %>%
  select(SUBJECT_ID, F09Q01) %>%
  rename(mrs.30 = F09Q01)

atachmrs90 <- atach09 %>%
  filter(mrsday == 90) %>%
  select(SUBJECT_ID, F09Q01) %>%
  rename(mrs.90 = F09Q01)

atach.whole <-left_join(atach.whole, atachmrs30, by = "SUBJECT_ID")
atach.whole <-left_join(atach.whole, atachmrs90, by = "SUBJECT_ID")

# Categorizing and Recoding Variables

# Race
atach.whole <- atach.whole %>%
  mutate(race = case_when(
    F01Q03M1 == 1 ~ "American Indian or Alaskan Native",
    F01Q03M2 == 1 ~ "Asian",
    F01Q03M3 == 1 ~ "Black or African-American",
    F01Q03M5 == 1 ~ "White",
    F01Q03M6 == 1 ~ "Other/Not Reported",
    F01Q03M7 == 1 ~ "Other/Not Reported"
  ))

# Ethnicity
atach.whole <- atach.whole %>% 
  mutate(ethnicity = recode(F01Q02,
                             "1" = "Hispanic or Latino",
                             "2" = "Not Hispanic or Latino",
                             .default = "Unknown/Not Reported"
                             ))

# Sex
atach.whole <- atach.whole %>% 
  mutate(sex = dplyr::recode(F01Q01,
                      "1" = "Male",
                      "2" = "Female"))

# Country of Enrolling Site
atach.whole <- atach.whole %>% 
  mutate(enrolling.country = recode(COUNTRY,
                                    "1" = "United States",
                                    "2" = "Japan",
                                    "3" = "China",
                                    "4" = "South Korea",
                                    "5" = "Taiwan",
                                    "6" = "Germany"))

# Past Medical History
atach.whole <- atach.whole %>%
  mutate(stroke = recode(F03Q01,
                         "0" = "No",
                         "1" = "Yes",
                         "98" = "NA"))

atach.whole <- atach.whole %>%
  mutate(chf = recode(F03Q03,
                         "0" = "No",
                         "1" = "Yes",
                         "98" = "NA"))

atach.whole <- atach.whole %>%
  mutate(afib = recode(F03Q04,
                         "0" = "No",
                         "1" = "Yes",
                         "98" = "NA"))

atach.whole <- atach.whole %>%
  mutate(cad = case_when(
    F03Q05 == 0 ~ "No",
    F03Q05 == 1 ~ "Yes",
    F03Q05 == 98 ~ "NA",
    F03Q06 == 0 ~ "No",
    F03Q06 == 1 ~ "Yes",
    F03Q06 == 98 ~ "NA"
  ))

atach.whole <- atach.whole %>%
  mutate(htn = recode(F03Q07,
                         "0" = "No",
                         "1" = "Yes",
                         "98" = "NA"))

atach.whole <- atach.whole %>%
  mutate(pvd = recode(F03Q08,
                         "0" = "No",
                         "1" = "Yes",
                         "98" = "NA"))

atach.whole <- atach.whole %>%
  mutate(hld = recode(F03Q09,
                         "0" = "No",
                         "1" = "Yes",
                         "98" = "NA"))

atach.whole <- atach.whole %>%
  mutate(dm2 = recode(F03Q12,
                         "0" = "No",
                         "1" = "Yes",
                         "98" = "NA"))

atach.whole <- atach.whole %>%
  mutate(tobacco = recode(F03Q13,
                         "0" = "No",
                         "1" = "Yes",
                         "98" = "NA"))

# ICH Laterality
atach.whole <- atach.whole %>%
  mutate(ich.laterality = recode(ich.location.baseline,
                                 "1" = "Right",
                                 "2" = "Right",
                                 "3" = "Right",
                                 "4" = "Left",
                                 "5" = "Left",
                                 "6" = "Left",
                                 .default = "NA"))

# ICH Location
atach.whole <- atach.whole %>%
  mutate(ich.location.baseline = recode(ich.location.baseline,
                                        "1" = "Thalamus",
                                        "2" = "Basal Ganglia",
                                        "3" = "Lobar",
                                        "4" = "Thalamus",
                                        "5" = "Basal Ganglia",
                                        "6" = "Lobar",
                                        "7" = "Pons",
                                        "8" = "Cerebellum"))

atach.whole <- atach.whole %>%
  mutate(ivh = recode(ivh.baseline,
                         "0" = "No",
                         "1" = "Yes",
                         "98" = "NA"))

atach.whole <- atach.whole %>%
  mutate(hydrocephalus = recode(hydrocephalus.baseline,
                         "0" = "No",
                         "1" = "Yes",
                         "98" = "NA"))

atach.whole <- atach.whole %>%
  mutate(hydrocephalus = recode(hydrocephalus.baseline,
                         "0" = "No",
                         "1" = "Yes",
                         "98" = "NA"))

atach.whole <- atach.whole %>%
  mutate(ich.volume.cc = ich.volume.baseline/1000)

atach.whole <- atach.whole %>%
  mutate(phe.volume.cc = phe.volume.baseline/1000)

atach.whole <- atach.whole %>%
  mutate(ivh.volume.cc = ivh.volume.baseline/1000)

atach.whole <- atach.whole %>%
  mutate(mech.ventilation = recode(F07Q01,
                                   "0" = "No",
                                   "1" = "Yes",
                                   .default = "NA"))

atach.whole <- atach.whole %>%
  mutate(evd = recode(F07Q05,
                                   "0" = "No",
                                   "1" = "Yes",
                                   .default = "NA"))

atach.whole <- atach.whole %>%
  mutate(nsgy.evacuation = recode(F07Q08,
                                   "0" = "No",
                                   "1" = "Yes",
                                   .default = "NA"))

atach.whole <- atach.whole %>%
  mutate(tracheostomy = recode(F07Q11,
                                   "0" = "No",
                                   "1" = "Yes",
                                   .default = "NA"))

# NIHSS
atach.nihss.baseline <- atach10 %>%
  filter(nihss.assessment == "baseline") %>%
  select(SUBJECT_ID, F10Q22) %>%
  rename(nihss.baseline = F10Q22)

atach.nihss.24hourpost <- atach10 %>%
  filter(nihss.assessment == "24hourpost") %>%
  select(SUBJECT_ID, F10Q22) %>%
  rename(nihss.24hourpost = F10Q22)
  

atach.whole <-left_join(atach.whole, atach.nihss.baseline, by = "SUBJECT_ID")
atach.whole <-left_join(atach.whole, atach.nihss.24hourpost, by = "SUBJECT_ID")


# EuroQol

atach.whole <- atach.whole %>%
  mutate(euro.mobility.90 = factor(F16Q02,
                                labels = c("No Problems", "Some Problems", "Confined to Bed")))

atach.whole <- atach.whole %>%
  mutate(euro.selfcare.90 = factor(F16Q03,
                                labels = c("No Problems", "Some Problems", "Unable to Wash or Dress Myself")))

atach.whole <- atach.whole %>%
  mutate(euro.usual.90 = factor(F16Q04,
                                labels = c("No Problems", "Some Problems", "Unable to Perform My Usual Activities")))

atach.whole <- atach.whole %>%
  mutate(euro.pain.90 = factor(F16Q05,
                                labels = c("No Pain/Discomfort", "Some Moderate Pain/Discomfort", "Extreme Pain/Discomfort")))

atach.whole <- atach.whole %>%
  mutate(euro.anxiety.90 = factor(F16Q06,
                                labels = c("Not Anxious/Depressed", "Moderately Anxious/Depressed", "Extremely Anxious/Depressed")))


# Filtering out cases without ICH laterality
atach.whole <- atach.whole %>%
  filter(ich.laterality != "NA")

# Renaming variables

atach.whole <- atach.whole %>%
  rename(age = F33Q02) %>%
  rename(symptom.onset = F00Q02MIN) %>% # All times in ATACH2 dataset are expressed as minutes from randomization
  rename(arrivaltime.initial = F02Q03MIN) %>% 
  rename(arrivaltime.strokecenter = F02Q05MIN) %>%
  rename(sbp.initial = F05Q07A) %>%
  rename(dbp.initial = F05Q07B) %>%
  rename(gcs.baseline = F33Q04) %>%
  rename(intubation.days = F07Q02) %>%
  rename(extubation.date = F07Q03DAY) %>%
  rename(trach.date = F07Q04DAY) %>%
  rename(evd.days = F07Q07) %>%
  rename(nsgy.evacuation.date = F07Q09DAY) %>%
  rename(euro.vas.90 = F16Q07) %>%
  rename(EQ_INDEX.90 = EQ_INDEX)

# Calculating Time to ED Presentation as expressed by minutes and hours from symptom onset

atach.whole <- atach.whole %>%
  mutate(time.to.ed.minutes = abs(arrivaltime.strokecenter - symptom.onset)) %>%
  mutate(time.to.ed.hours = time.to.ed.minutes / 60)

atach.whole$time.to.ed.hours <- round(atach.whole$time.to.ed.hours, digits = 2)

atach.times <- atach.whole %>%
  select(c(symptom.onset, arrivaltime.initial, arrivaltime.strokecenter, time.to.ed.minutes, time.to.ed.hours))

# DNR and WLST status

atach.whole <- atach.whole %>%
  mutate(dnr.binary = case_when(
    F21Q08 == "0" ~ "No",
    F21Q08 == "1" ~ "Yes"
  ))

atach.whole <- atach.whole %>%
  mutate(comfort.binary = case_when(
    F21Q06 == "0" ~ "No",
    F21Q06 == "1" ~ "Yes"
  ))

atach.whole <- atach.whole %>%
  rename(comfort.day = F21Q07DAY) %>%
  rename(dnr.day = F21Q09DAY)
