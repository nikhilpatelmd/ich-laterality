library(ggplot2)
library(tidybayes)
library(dplyr)
library(stringr)
library(patchwork)
library(rlang) # Added for the injection operator

f_posterior_diagnostics <- function(mod, param_regex = "ich_lateralityRight") {
  # 1. Get all available variable names directly from the brms object
  all_vars <- tidybayes::get_variables(mod)

  # 2. Filter down to only the exact string names that match your regex
  target_vars <- all_vars[stringr::str_detect(all_vars, param_regex)]

  # 3. Safely exit if no parameters match
  if (length(target_vars) == 0) {
    warning(paste("No parameters matched the regex:", param_regex))
    return(NULL)
  }

  # 4. THE FIX: Inject the target variables directly as bare names using !!!rlang::syms()
  draws <- mod |>
    tidybayes::gather_draws(!!!rlang::syms(target_vars))

  # 5. Trace Plot
  trace_plot <- draws |>
    ggplot(aes(x = .iteration, y = .value, color = factor(.chain))) +
    geom_line(linewidth = 0.2, alpha = 0.8) +
    scale_color_viridis_d(option = "rocket", end = 0.85, name = "Chain") +
    facet_wrap(vars(.variable), scales = "free_y", ncol = 1) +
    theme_minimal() +
    labs(x = "Iteration", y = "Parameter Value", title = "Trace Plots") +
    theme(legend.position = "none")

  # 6. Trank Plot
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

  # 7. Combine side-by-side
  return(trace_plot | trank_plot)
}
