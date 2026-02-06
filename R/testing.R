# Plot the Expected Value (Mean VAS) for Left vs Right
plot_predictions(
  m_vas_90,
  condition = "ich_laterality",
  type = "response"
) +
  labs(
    y = "Predicted Mean VAS Score (0-1 scale)",
    x = "ICH Laterality",
    title = "Expected EuroQOL VAS by Laterality"
  ) +
  theme_minimal()

library(marginaleffects)
