library(tidyverse)
library(qs)
library(data.table)
library(Hmisc)
library(qreport)

getwd()

d <- qread("./data/nindsCombined.qs")

mrsCounts <- d |>
  filter(!is.na(mrs90)) |>
  group_by(ichLaterality, mrs90) |>
  tally() |>
  dplyr::mutate(percent = scales::percent(n / sum(n), accuracy = 1, trim = FALSE))

dlist <- c(d$mrs30, d$mrs90)

missChk(dlist)

str(d$euroMobility90)

fct_count(d$euroSelfCare365)
