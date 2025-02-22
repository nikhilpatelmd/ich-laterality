# https://octavio.me/posts/ordinal-viz/

library(tidyverse)
library(brms)

# Create a new data frame with the predictor values
newdata <- data.frame(
  ich_laterality = c("Left", "Right"),
  age = mean(m_posterior_neutral_mrs_90_canonical$data$age),  # Use mean age from the original data
  gcs_baseline = mean(m_posterior_neutral_mrs_90_canonical$data$gcs_baseline),  # Use mean GCS from the original data
  ich_location = "Basal Ganglia",  # Choose a reference level for ich_location
  ich_volume_baseline = mean(m_posterior_neutral_mrs_90_canonical$data$ich_volume_baseline),  # Use mean ICH volume from the original data
  ivh = "No",  # Choose a reference level for ivh
  time_symptoms_to_ed = mean(m_posterior_neutral_mrs_90_canonical$data$time_symptoms_to_ed)  # Use mean time to ED from the original data
)

# Generate predictions for each level of mrs_90
predictions <- posterior_predict(
  m_posterior_neutral_mrs_90_canonical,
  newdata = newdata,
  category = "mrs_90"  # Specify the category for the predictions
)

# Transform predictions to probabilities
probabilities <- apply(predictions, 2, function(x) {
  prop.table(table(factor(x, levels = 0:6, ordered = TRUE)))
})

# Convert probabilities to a data frame suitable for plotting
probabilities_df <- probabilities %>%
  as.data.frame() %>%
  rownames_to_column("mrs_90") %>%
  pivot_longer(
    cols = starts_with("V"),
    names_to = "ich_laterality",
    values_to = "probability"
  ) %>%
  mutate(
    mrs_90 = factor(mrs_90, levels = 0:6, ordered = TRUE),
    ich_laterality = ifelse(ich_laterality == "V1", "Left", "Right")
  )

# Plot the cumulative probabilities
ggplot(probabilities_df, aes(x = probability, y = ich_laterality, color = mrs_90)) +
  geom_line(aes(group = mrs_90)) +
  geom_point() +
  theme_bw() +
  labs(
    x = "Probability",
    y = "Hemispheric Laterality",
    color = "mRS"
  )
