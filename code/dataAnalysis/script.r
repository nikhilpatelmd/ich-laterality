library(tidyverse)

getwd()

d <- qread("./data/nindsCombined.qs")

mrsCounts <- d |>
  filter(!is.na(mrs90)) |>
  group_by(ichLaterality, mrs90) |>
  tally() |>
  dplyr::mutate(percent = scales::percent(n / sum(n), accuracy = 1, trim = FALSE))