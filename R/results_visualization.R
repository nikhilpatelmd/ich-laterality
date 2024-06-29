results_visual <- function(model, title) {
  model <- model
  subtitle <- title

  draws <- model |>
    spread_draws(b_ich_lateralityRight) |>
    mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
    summarize()

  pred <- predictions(model, by = "ich_laterality") |>
    tibble()

  theme_ich <- function(base_family = "Inter",
                        base_size = 14) {
    theme_minimal(base_size = base_size) +
      theme(
        text = element_text(lineheight = 2),
        plot.title = element_text(size = rel(1.3), margin = margin(12, 0, 8, 0)),
        plot.subtitle = element_text(size = rel(1.1), margin = margin(4, 0, 0, 0)),
        axis.text.y = element_text(size = rel(0.8)),
        axis.title.y = element_text(size = 12, margin = margin(0, 4, 0, 0)),
        axis.text.x = element_text(size = rel(2)),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.justification = 1,
        panel.grid = element_line(colour = "#F3F4F5"),
        plot.caption = element_text(size = rel(0.8), margin = margin(8, 0, 0, 0)),
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm")
      )
  }

  p1 <- predictions(model, by = "ich_laterality") |>
    posterior_draws() |>
    ggplot(aes(x = draw, fill = ich_laterality)) +
    geom_density(alpha = .7) +
    scale_fill_manual(values = c("#054224", "#ff8800")) +
    scale_x_continuous(labels = scales::percent) +
    labs(
      title = "Predicted Probability",
      fill = "ICH Laterality",
      y = NULL
    ) +
    theme_ich()

  p2 <- avg_comparisons(model, variables = "ich_laterality") |>
    posterior_draws() |>
    ggplot(aes(x = draw)) +
    geom_density(color = "#054224", fill = "#ff8800") +
    scale_x_continuous(labels = scales::percent) +
    labs(
      title = "Average Difference between Left vs. Right Hemisphere",
      y = NULL
    ) +
    geom_vline(
      xintercept = 0, linetype = "dashed",
      color = "#999999", linewidth = 1
    ) +
    theme_ich()

  p3 <- model |>
    spread_draws(b_ich_lateralityRight) |>
    mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
    ggplot(aes(ich_right_or)) +
    geom_density(color = "#054224", fill = "#ff8800") +
    # stat_halfeye(aes(fill_ramp = stat(x > 1), fill = "#E69F00", color = "#422f05")) +
    scale_x_continuous(breaks = seq(0, 3, 0.25)) +
    labs(
      title = "Odds Ratio - Right Hemisphere",
      y = NULL,
      fill = NULL
    ) +
    geom_vline(
      xintercept = 1, linetype = "dashed",
      color = "#999999", linewidth = 1
    ) +
    theme_ich() +
    theme(legend.position = "none")


  total <- p1 / p2 / p3

  final <- total +
    plot_annotation(
      title = glue(subtitle),
      theme = theme(plot.title = element_text(size = rel(2), margin = margin(12, 0, 8, 0)))
    )

  return(final)
}
