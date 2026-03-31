library(tidyverse)
library(marginaleffects)
library(patchwork)

make_euroqol_facet_plot <- function(model, title_label) {
  # 1. Extract posterior probabilities for all 3 levels
  # Level 1: No, 2: Some, 3: Significant
  plot_data <- avg_predictions(
    model,
    by = c("ich_laterality", "group"),
    conf_level = 0.95
  ) |>
    as_tibble() |>
    mutate(
      level_label = case_when(
        group == 1 ~ "1: No Problems",
        group == 2 ~ "2: Some Problems",
        group == 3 ~ "3: Significant Problems"
      ),
      # Ensure levels are in order for faceting
      level_label = fct_reorder(level_label, as.numeric(as.character(group)))
    )

  # 2. Generate the faceted plot
  ggplot(
    plot_data,
    aes(x = estimate, y = ich_laterality, color = ich_laterality)
  ) +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 0.8) +
    facet_wrap(~level_label, scales = "free_x") +
    scale_x_continuous(
      labels = scales::percent,
      name = "Predicted Probability"
    ) +
    scale_color_manual(values = c("Left" = "#ce4951", "Right" = "#476170")) + # Using your project colors
    labs(title = title_label, y = NULL) +
    theme_minimal() +
    theme(legend.position = "none")
}
