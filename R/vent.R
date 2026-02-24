library(ggplot2)

ich_aggressive |>
  ggplot(
    aes(x = days_mechanical_ventilation)
  ) +
  geom_histogram() +
  theme_minimal(
    base_size = 16,
    base_family = "Liberation Sans"
  )
