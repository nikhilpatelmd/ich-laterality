# Format a posterior probability for display, handling the finite-sample
# ceiling problem. With N draws, the finest resolution is 1/N, so claiming
# exactly 0% or 100% overstates certainty. Instead we report "> 99.9%" or
# "< 0.1%" when the probability hits those boundaries.
#
# Arguments:
#   p       : a probability between 0 and 1 (NOT already multiplied by 100)
#   digits  : decimal places to display (default 1)
#
# Returns a formatted string like "97.3%", "> 99.9%", or "< 0.1%"

format_posterior_prob <- function(p, digits = 1) {
  upper_threshold <- 1 - (1 / 1000) # 99.9%
  lower_threshold <- 1 / 1000 # 0.1%

  dplyr::case_when(
    p >= upper_threshold ~ "> 99.9%",
    p <= lower_threshold ~ "< 0.1%",
    TRUE ~ paste0(round(p * 100, digits), "%")
  )
}
