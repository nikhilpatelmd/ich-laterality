euro_figure_function <- function(x, var) {
  theme_ich <- function(base_size = 16) {
    theme_minimal(base_size = base_size) +
      theme(
        text = element_text(size = 20, lineheight = 2),
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
          size = rel(3),
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

  data <- x |>
    select(ich_laterality, {{ var }}) |>
    na.omit() |>
    count({{ var }}, ich_laterality) |>
    group_by(ich_laterality) |>
    mutate(
      pct_n = n / sum(n),
      pct_label = percent(pct_n, accuracy = 1),
      euro = case_when(
        {{ var }} == 1 ~ "1: No Problems",
        {{ var }} == 2 ~ "2: Some Problems",
        {{ var }} == 3 ~ "3: Significant Problems"
      ),
      euro = fct_rev(euro)
    )

  data |>
    ggplot(aes(
      x = ich_laterality,
      y = pct_n,
      fill = euro
    )) +
    geom_col(width = 0.5) +
    geom_text(aes(label = pct_label),
      position = position_stack(vjust = 0.5),
      color = "white",
      fontface = "bold",
      size = 10
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
        "1: No Problems",
        "2: Some Problems",
        "3: Significant Problems"
      ),
      values = c("#eec6a2", "#d98174", "#ce4950")
    ) +
    labs(
      x = NULL,
      y = NULL,
      fill = "EuroQOL at 90 days"
    ) +
    theme_ich()
}
