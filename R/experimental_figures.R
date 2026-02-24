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



library(tidyverse)
library(brms)
library(tidybayes)
library(ggrepel)
library(viridis)

# Define mRS labels
mrs_labels <- c(
  "0 - No symptoms",
  "1 - No significant disability",
  "2 - Slight disability",
  "3 - Moderate disability",
  "4 - Moderately severe",
  "5 - Severe disability",
  "6 - Dead"
)

# Simulating some clinical trial data (e.g., Age and Treatment predicting mRS)
set.seed(42)
n <- 500
stroke_data <- tibble(
  age = rnorm(n, mean = 65, sd = 12),
  treatment = sample(c("Control", "Treatment"), n, replace = TRUE),
  # Simulating a latent continuous variable z that determines the ordinal outcome
  z = 0.05 * (age - 65) - 0.8 * (treatment == "Treatment") + rnorm(n),
  mrs_num = as.numeric(cut(
    z,
    breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, 2.5, 3.5, Inf)
  )) -
    1
) %>%
  mutate(
    # Crucial step: mRS must be an ordered factor for the cumulative model
    mrs = factor(mrs_num, levels = 0:6, labels = mrs_labels, ordered = TRUE)
  )

fit_mrs <- brm(
  formula = mrs ~ age + treatment,
  data = stroke_data,
  family = cumulative(link = "logit"),
  cores = 4,
  chains = 4
)

# Create a grid of values we want to predict over
pred_grid <- tibble(
  age = seq(30, 95, length.out = 50),
  treatment = "Treatment" # Holding treatment constant for the plot
)

# Extract expected predictions (probabilities)
mrs_preds <- pred_grid %>%
  add_epred_draws(fit_mrs, ndraws = 100) %>%
  ungroup() %>%
  # Crucial: create a unique index for ggplot to group the individual draw lines
  mutate(indices = paste(.draw, .category, sep = "_"))

# Plotting
# Viridis 'turbo' or 'plasma' works well to distinguish 7 categories
p_continuous <- mrs_preds %>%
  ggplot(aes(x = age, y = .epred, color = .category, group = indices)) +
  geom_line(alpha = 0.1) +
  scale_color_viridis_d(option = "plasma", end = 0.9) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Predicted Probability of mRS Scores by Age",
    subtitle = "Posterior draws for patients in the Treatment arm",
    x = "Age (years)",
    y = "Probability"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    legend.position = "none",
    # Make room on the right margin for the ggrepel labels
    plot.margin = margin(10, 150, 10, 10)
  ) +
  coord_cartesian(clip = "off") # Prevent labels from being cut off

# Add direct labels to the lines at the maximum age
p_continuous +
  geom_text_repel(
    data = mrs_preds %>%
      filter(age == max(age)) %>%
      distinct(.category, .keep_all = TRUE),
    aes(label = .category),
    direction = "y",
    hjust = 0,
    nudge_x = 2,
    segment.size = 0.2,
    size = 3.5
  )

# Grid for discrete comparison (holding age at the mean)
discrete_grid <- tibble(
  treatment = c("Control", "Treatment"),
  age = 65
)

# Get the median expected probability for each category
mrs_bar_preds <- discrete_grid %>%
  add_epred_draws(fit_mrs) %>%
  group_by(treatment, .category) %>%
  summarise(
    prob = median(.epred),
    .groups = "drop"
  )

# Plotting an adapted modeled Grotta Bar
mrs_bar_preds %>%
  ggplot(aes(y = treatment, x = prob, fill = fct_rev(.category))) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_viridis_d(option = "mako", direction = -1, name = "mRS Score") +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Modeled mRS Distribution by Treatment Arm",
    subtitle = "Adjusted for Age = 65",
    x = "Proportion of Patients",
    y = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

library(tidyverse)
library(brms)
library(marginaleffects)
library(ggrepel)

# 1. Generate predictions across a range of ages using datagrid()
# By default, marginaleffects computes the expected probability for each mRS level
preds_continuous <- predictions(
  fit_mrs,
  newdata = datagrid(age = seq(30, 95, by = 5), treatment = "Treatment")
)

# 2. Plot the estimates and credible ribbons
p_marg_cont <- preds_continuous %>%
  ggplot(aes(x = age, y = estimate, color = group)) +
  # Add the 95% credible interval ribbons
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = group),
    alpha = 0.1,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  scale_color_viridis_d(option = "plasma", end = 0.9) +
  scale_fill_viridis_d(option = "plasma", end = 0.9) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Predicted Probability of mRS Scores by Age",
    subtitle = "Median estimates and 95% credible intervals (Treatment arm)",
    x = "Age (years)",
    y = "Probability"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 150, 10, 10)
  ) +
  coord_cartesian(clip = "off")

# Add direct labels as before
p_marg_cont +
  geom_text_repel(
    data = preds_continuous %>%
      filter(age == max(age)),
    aes(label = group),
    direction = "y",
    hjust = 0,
    nudge_x = 2,
    segment.size = 0.2,
    size = 3.5
  )

# 1. Generate predictions for the discrete groups
preds_discrete <- predictions(
  fit_mrs,
  newdata = datagrid(treatment = c("Control", "Treatment"))
)

# 2. Plot the Grotta Bar
preds_discrete %>%
  ggplot(aes(y = treatment, x = estimate, fill = fct_rev(group))) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_viridis_d(option = "mako", direction = -1, name = "mRS Score") +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Modeled mRS Distribution by Treatment Arm",
    subtitle = "Adjusted predictions for a 65-year-old patient",
    x = "Expected Proportion",
    y = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

library(tidyverse)
library(brms)
library(marginaleffects)

# Calculate average predicted probabilities by treatment group
# This averages over the observed age distribution in your dataset
adj_probs <- predictions(
  fit_mrs,
  by = "treatment"
)

# You can inspect the output: notice 'estimate' is a probability, and 'group' is the mRS level
head(adj_probs)

adj_probs %>%
  ggplot(aes(y = treatment, x = estimate, fill = fct_rev(group))) +
  geom_col(position = position_stack(reverse = TRUE), width = 0.6) +
  scale_fill_viridis_d(option = "mako", direction = -1, name = "mRS Score") +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Covariate-Adjusted mRS Distribution",
    subtitle = "Population-averaged predicted probabilities",
    x = "Proportion of Patients",
    y = NULL
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )

# Calculate the Average Marginal Effect of Treatment vs. Control
ames <- avg_comparisons(
  fit_mrs,
  variables = "treatment"
)

# View the results
ames %>%
  select(group, term, contrast, estimate, conf.low, conf.high) %>%
  mutate(across(
    c(estimate, conf.low, conf.high),
    ~ scales::percent(.x, accuracy = 0.1)
  ))
