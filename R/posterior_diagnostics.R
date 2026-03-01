library(ggplot2)
library(tidybayes)
library(dplyr)
library(stringr)
library(patchwork)

f_posterior_diagnostics <- function(mod, param_regex = "ich_lateralityRight") {
  # 1. Grab all fixed effects (b_) and random effect SDs (sd_),
  # then filter down to the specific parameter(s) you care about.
  draws <- mod |>
    tidybayes::gather_draws(`^b_.*`, `^sd_.*`, regex = TRUE) |>
    dplyr::filter(stringr::str_detect(.variable, param_regex))

  if (nrow(draws) == 0) {
    warning(paste("No parameters matched the regex:", param_regex))
    return(NULL)
  }

  # 2. Trace Plot
  trace_plot <- draws |>
    ggplot(aes(x = .iteration, y = .value, color = factor(.chain))) +
    geom_line(linewidth = 0.2, alpha = 0.8) +
    scale_color_viridis_d(option = "rocket", end = 0.85, name = "Chain") +
    facet_wrap(vars(.variable), scales = "free_y", ncol = 1) +
    theme_minimal() +
    labs(x = "Iteration", y = "Parameter Value", title = "Trace Plots") +
    theme(legend.position = "none") # Hide legend to save space

  # 3. Trank Plot
  trank_plot <- draws |>
    group_by(.variable) |>
    mutate(draw_rank = rank(.value)) |>
    ggplot(aes(x = draw_rank, color = factor(.chain))) +
    stat_bin(
      geom = "step",
      binwidth = 200,
      position = position_identity(),
      boundary = 0,
      linewidth = 0.6
    ) +
    scale_color_viridis_d(option = "rocket", end = 0.85, name = "Chain") +
    facet_wrap(vars(.variable), scales = "free_y", ncol = 1) +
    theme_minimal() +
    labs(x = "Rank", y = "Count", title = "Rank Histograms") +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank()
    )

  # 4. Combine side-by-side
  return(trace_plot | trank_plot)
}
