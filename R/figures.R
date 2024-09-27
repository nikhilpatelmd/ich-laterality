subgroup_figure_function <- function(model) {
  left_fill <- "#f69d75"
  right_fill <- "#551e4e"

  theme_ich <- function(base_size = 10) {
    theme_minimal(base_size = base_size) +
      theme(
        text = element_text(lineheight = 2),
        plot.title = element_text(size = rel(2), margin = margin(12, 0, 8, 0)),
        plot.subtitle = element_text(size = rel(1.1), margin = margin(4, 0, 0, 0)),
        axis.text.y = element_blank(),
        axis.title.y = element_text(
          size = rel(2),
          margin = margin(0, 40, 0, 0),
          vjust = 0.5
        ),
        axis.text.x = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5), margin = margin(10, 0, 0, 0)),
        strip.text.x = element_text(
          size = rel(2),
          margin = margin(0, 0, 20, 0),
          vjust = 0
        ),
        legend.position = "bottom",
        legend.justification = 1,
        panel.grid = element_blank(),
        plot.caption = element_text(size = rel(1), margin = margin(8, 0, 0, 0)),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
      )
  }

  total_plot <- predictions(model, type = "response", by = c("ich_laterality")) |>
    posterior_draws() |>
    ggplot(aes(x = draw, fill = ich_laterality)) +
    geom_density(alpha = .5) +
    # facet_grid(~ich_location) +
    scale_fill_manual(values = c(left_fill, right_fill)) +
    scale_x_continuous(
      limits = c(0, 0.15),
      breaks = seq(0, 0.15, 0.025),
      labels = scales::percent
    ) +
    labs(
      title = "Overall Probability",
      x = NULL,
      y = NULL,
      fill = "Hemispheric Laterality"
    ) +
    theme_ich()

  ich_location_plot <- predictions(model, type = "response", by = c("ich_laterality", "ich_location")) |>
    posterior_draws() |>
    ggplot(aes(x = draw, fill = ich_laterality)) +
    geom_density(alpha = .5) +
    facet_grid(~ich_location) +
    scale_color_manual(values = c(left_fill, right_fill)) +
    scale_fill_manual(values = c(left_fill, right_fill)) +
    scale_x_continuous(
      limits = c(0, 0.2),
      breaks = seq(0, 0.2, 0.05),
      labels = scales::percent
    ) +
    labs(
      title = "ICH Location",
      x = NULL,
      y = NULL,
      fill = "Hemispheric Laterality"
    ) +
    theme_ich()

  ich_study_plot <- predictions(model, type = "response", by = c("ich_laterality", "study")) |>
    posterior_draws() |>
    ggplot(aes(x = draw, fill = ich_laterality)) +
    geom_density(alpha = .5) +
    facet_grid(~study) +
    scale_color_manual(values = c(left_fill, right_fill)) +
    scale_fill_manual(values = c(left_fill, right_fill)) +
    scale_x_continuous(
      limits = c(0, 0.2),
      breaks = seq(0, 0.2, 0.05),
      labels = scales::percent
    ) +
    labs(
      title = "Study",
      x = NULL,
      y = NULL,
      fill = "Hemispheric Laterality"
    ) +
    theme_ich()

  ich_age_plot <- plot_predictions(model, condition = c("age", "ich_laterality")) +
    scale_color_manual(
      values = c(left_fill, right_fill)
    ) +
    scale_fill_manual(
      values = c(left_fill, right_fill)
    ) +
    scale_x_continuous(
      limits = c(20, 95),
      breaks = seq(20, 95, 10)
    ) +
    labs(
      title = "Age",
      x = "Age",
      y = NULL,
      fill = "Hemispheric Laterality"
    ) +
    theme_ich() +
    theme(legend.position = "none")

  ich_volume_plot <- plot_predictions(model, condition = c("ich_volume_baseline", "ich_laterality")) +
    scale_color_manual(
      values = c(left_fill, right_fill)
    ) +
    scale_fill_manual(
      values = c(left_fill, right_fill)
    ) +
    scale_x_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 10)
    ) +
    labs(
      title = "ICH Volume",
      x = "Baseline ICH Volume (mL)",
      y = NULL
    ) +
    theme_ich() +
    theme(legend.position = "none")

  final_figure <- (total_plot / ich_location_plot / (ich_age_plot + ich_volume_plot) / ich_study_plot) +
    plot_annotation(tag_levels = "A") +
    plot_layout(
      guides = "collect"
    ) &
    theme(legend.position = "bottom", legend.justification = 0)

  return(final_figure)
}

mrs_figure_function <- function(model) {
  theme_ich <- function(base_size = 10) {
    theme_minimal(base_size = base_size) +
      theme(
        text = element_text(lineheight = 2),
        plot.title = element_text(size = rel(2), margin = margin(12, 0, 8, 0)),
        plot.subtitle = element_text(size = rel(1.1), margin = margin(4, 0, 0, 0)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(
          size = rel(2),
          margin = margin(0, 40, 0, 0),
          vjust = 0.5
        ),
        axis.text.x = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5), margin = margin(10, 0, 0, 0)),
        strip.text.x = element_text(
          size = rel(2),
          margin = margin(20, 0, 20, 0),
          vjust = 0.5
        ),
        legend.title = element_text(margin = margin(0, 20, 0, 0)),
        legend.position = "bottom",
        legend.justification = 0.5,
        panel.grid = element_blank(),
        plot.caption = element_text(size = rel(1), margin = margin(8, 0, 0, 0)),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
      )
  }

  mod <- avg_predictions(model, by = "ich_laterality")

  data <- mod |>
    select(group, ich_laterality, estimate) |>
    as_tibble() |>
    mutate(
      estimate_label = percent(estimate, accuracy = 1),
      mrs = case_when(
        group == 0 ~ "0: No symptoms",
        group == 1 ~ "1: No significant disability",
        group == 2 ~ "2: Slight disability",
        group == 3 ~ "3: Moderate disability",
        group == 4 ~ "4: Moderately severe disability",
        group == 5 ~ "5: Severe disability",
        group == 6 ~ "6: Dead"
      ),
      mrs = fct_rev(mrs)
    )

  data |>
    ggplot(aes(
      x = ich_laterality,
      y = estimate,
      fill = mrs
    )) +
    geom_col(width = 0.5) +
    geom_text(aes(label = estimate_label),
      position = position_stack(vjust = 0.5),
      color = "white",
      fontface = "bold"
    ) +
    coord_flip() +
    scale_x_discrete() +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.1),
      labels = scales::percent
    ) +
    scale_fill_manual(
      breaks = c(
        "0: No symptoms",
        "1: No significant disability",
        "2: Slight disability",
        "3: Moderate disability",
        "4: Moderately severe disability",
        "5: Severe disability",
        "6: Dead"
      ),
      values = generate_palette(c(72, 198, 238),
        blend_colour = c(118, 75, 162),
        n_colours = 7
      )
    ) +
    labs(
      x = NULL,
      y = NULL,
      fill = "mRS at 90 days"
    ) +
    theme_ich()
}
