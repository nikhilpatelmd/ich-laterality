library(tidyverse)
library(tidybayes)
library(ggridges)

# List of your models
models <- list(
  "Neutral Prior" = m_posterior_neutral_neurosurgery,
  "Left Prior" = m_posterior_left_neurosurgery,
  "Right Prior" = m_posterior_right_neurosurgery,
  "Flat Prior" = m_posterior_flat_neurosurgery
)

# Function to extract and transform draws for a single model
extract_draws <- function(model, coefficient_name, label, model_name) {
  model |>
    gather_draws(`^b_.*`, regex = TRUE) |>
    mutate(.value = exp(.value)) |>
    filter(.variable == coefficient_name) |>
    mutate(
    .variable = factor(.variable, levels = coefficient_name, labels = label, ordered = TRUE),
      model_name = model_name
    )
}

# Apply the function to each model and combine the results
all_draws <- imap_dfr(
  models,
  ~ extract_draws(.x, "b_ich_lateralityRight", "Right Hemisphere",.y)
)

# 1. Define the desired order of model names (REVERSED)
model_order <- rev(c("Neutral Prior", "Left Prior", "Right Prior", "Flat Prior"))

# 2. Convert 'model_name' to a factor with the specified order
all_draws$model_name <- factor(all_draws$model_name, levels = model_order)

rope_lower <- 0.95
rope_upper <- 1.05

# Calculate aOR and 95% CI for each model
aOR_CI <- all_draws %>%
  group_by(model_name) %>%
  median_qi(.value,.width = 0.95) %>%
  mutate(
    label = paste0("aOR = ", sprintf("%.2f",.value), " (", sprintf("%.2f",.lower), ", ", sprintf("%.2f",.upper), ")")
  )

# Plotting with ggridges
ggplot(all_draws, aes(x =.value, y = model_name, fill = model_name)) +
  geom_density_ridges(alpha = 0.5, scale = 0.9) +
  geom_rect(
    data = data.frame(x_min = rope_lower, x_max = rope_upper),
    aes(xmin = x_min, xmax = x_max, ymin = -Inf, ymax = Inf),
    alpha = 0.3, fill = "grey",
    inherit.aes = FALSE
  ) +
  scale_x_log10(breaks = seq(1, 3, by = 0.5)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_fill_viridis_d(option = "D") +
  scale_y_discrete(expand = c(0.001, 0.85)) +  # Add expand for margin
  labs(
    title = "Posterior Distributions of Odds Ratios",
    x = "Odds Ratio",
    y = "Model"
  ) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 10),
    legend.position = "none"
  ) +
  geom_text(
    data = aOR_CI,
    aes(x = 4, y = as.numeric(model_name) - 0.3, label = label), # Adjust position
    hjust = 1, vjust = -4.95, size = 4
  )

# Save the plot
ggsave(
  here("manuscripts","figure_1.png"), # Replace with your desired filename
  plot = last_plot(), # Saves the last plot generated
  width = 10,      # Adjust width as needed
  height = 8,     # Adjust height as needed
  dpi = 1200,       # Set the resolution (higher value = higher resolution)
  scale = 1        # Keep scale at 1 to preserve relative sizes
)
