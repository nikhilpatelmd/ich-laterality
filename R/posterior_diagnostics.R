posterior_diagnostics <- function(mod) {
  # plot code modified from Andrew Heiss (https://bayesf22-notebook.classes.andrewheiss.com/bayes-rules/13-chapter.html#simulating-the-posterior)

  trace_plot <- mod |>
    gather_draws(`^b_.*`, regex = TRUE) |>
    ggplot(aes(x = .iteration, y = .value, color = factor(.chain))) +
    geom_line(linewidth = 0.1) +
    scale_color_viridis_d(option = "rocket", end = 0.85) +
    facet_wrap(vars(.variable), scales = "free_y")

  trank_plot <- mod |>
    gather_draws(`^b_.*`, regex = TRUE) |>
    group_by(.variable) |>
    mutate(draw_rank = rank(.value)) |>
    ggplot(aes(x = draw_rank, color = factor(.chain))) +
    stat_bin(geom = "step", binwidth = 250, position = position_identity(), boundary = 0) +
    scale_color_viridis_d(option = "rocket", end = 0.85) +
    facet_wrap(vars(.variable), scales = "free_y") +
    theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank())

  # pp_check <- pp_check(mod, ndraws = 500)
  # pp_check_bars <- pp_check(mod, ndraws = 500, type = "bars_grouped", group = "ich_laterality")

  return((trank_plot / trace_plot))
}