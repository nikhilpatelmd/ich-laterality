tar_load(m_posterior_neutral_neurosurgery)
tar_load(ich_aggressive)

x <- ich_aggressive

# Subgroups for ICH Location

location_predictions <- predictions(
  m_posterior_neutral_neurosurgery,
  by = c("ich_laterality", "ich_location")
)

mean_difference_location <- avg_comparisons(
  m_posterior_neutral_neurosurgery,
  variables = "ich_laterality",  # AME for ich_laterality
  by = "ich_location",           # Stratified by ich_location
  type = "response"
)

location_predictions_plot <- location_predictions |>
    posterior_draws() |>
    ggplot(aes(x = estimate, fill = ich_laterality)) +
    geom_density(alpha = .5) +
    facet_grid(~ich_location) +
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
  theme_minimal() +
  theme(legend.position = "bottom")
  

study_predictions <- predictions(
  m_posterior_neutral_neurosurgery,
  by = c("ich_laterality", "study"),
  re_formula = NULL,     # Exclude random effects
)

mean_difference_study <- avg_predictions(
  m_posterior_neutral_neurosurgery,
  variables = "ich_laterality",  # AME for study
  by = "study",
  type = "response",    # AMEs on the probability scale
  re_formula = NULL,     # Exclude random effects
)

study_predictions_plot <- study_predictions |>
    posterior_draws() |>
    ggplot(aes(x = estimate, fill = ich_laterality)) +
    geom_density(alpha = .5) +
    facet_grid(~study) +
    scale_x_continuous(
      limits = c(0, 0.15),
      breaks = seq(0, 0.15, 0.05),
      labels = scales::percent
    ) +
    labs(
      title = "Study",
      x = NULL,
      y = NULL,
      fill = "Hemispheric Laterality"
    ) +
  theme_minimal() +
  theme(legend.position = "bottom") +

   # Add geom_text layer for annotations
   geom_text(
    data = as_tibble(mean_difference_study),
    aes(
      x = Inf, y = -Inf, 
      label = paste0(
        round(Estimate * 100, 1), "% (",
        round(`2.5 %` * 100, 1), "% - ",
        round(`97.5 %` * 100, 1), "%)"
      )
    ),
    hjust = 1.1, vjust = -1.5,  # Adjust position as needed
    inherit.aes = FALSE
  )

