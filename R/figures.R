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
