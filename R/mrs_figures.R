adjusted_mrs_function <- function(m_posterior_neutral_mrs_90_canonical) {
  pred <- avg_predictions(
    m_posterior_neutral_mrs_90_canonical,
    by = "ich_laterality"
  )

  pred <- as_tibble(pred) |>
    rename(
      mrs_90 = group,
      pct_n = estimate
    ) |>
    mutate(
      pct_label = percent(pct_n, accuracy = 1),
      mrs = case_when(
        mrs_90 == 0 ~ "0: No symptoms",
        mrs_90 == 1 ~ "1: No significant disability",
        mrs_90 == 2 ~ "2: Slight disability",
        mrs_90 == 3 ~ "3: Moderate disability",
        mrs_90 == 4 ~ "4: Moderately severe disability",
        mrs_90 == 5 ~ "5: Severe disability",
        mrs_90 == 6 ~ "6: Dead"
      ),
      mrs = fct_rev(mrs)
    ) |>
    select(mrs_90, ich_laterality, pct_n, pct_label, mrs)

  pred |>
    ggplot(aes(
      x = ich_laterality,
      y = pct_n,
      fill = mrs
    )) +
    geom_col(width = 0.5) +
    geom_text(
      aes(label = pct_label),
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
        "0: No symptoms",
        "1: No significant disability",
        "2: Slight disability",
        "3: Moderate disability",
        "4: Moderately severe disability",
        "5: Severe disability",
        "6: Dead"
      ),
      values = c(
        "#476170",
        "#7190a8",
        "#8bb4d0",
        "#b6dde5",
        "#eec6a2",
        "#d98174",
        "#ce4950"
      )
    ) +
    labs(
      x = NULL,
      y = NULL,
      fill = "mRS at 90 days"
    ) +
    theme_minimal() +

    # Add dotted lines using the segment_data (without aes())
    geom_segment(
      data = segment_data,
      x = 1.5,
      y = 0.1,
      xend = 1.5,
      yend = 0.9, # Directly specify coordinates
      color = "black",
      linetype = "dotted",
      inherit.aes = FALSE # Add this line
    )
}

mrs_figure_function <- function(x, var) {
  data <- x |>
    select(ich_laterality, {{ var }}) |>
    na.omit() |>
    count({{ var }}, ich_laterality) |>
    group_by(ich_laterality) |>
    mutate(
      pct_n = n / sum(n),
      pct_label = percent(pct_n, accuracy = 1),
      mrs = case_when(
        {{ var }} == 0 ~ "0: No symptoms",
        {{ var }} == 1 ~ "1: No significant disability",
        {{ var }} == 2 ~ "2: Slight disability",
        {{ var }} == 3 ~ "3: Moderate disability",
        {{ var }} == 4 ~ "4: Moderately severe disability",
        {{ var }} == 5 ~ "5: Severe disability",
        {{ var }} == 6 ~ "6: Dead"
      ),
      mrs = fct_rev(mrs)
    )

  data |>
    ggplot(aes(
      x = ich_laterality,
      y = pct_n,
      fill = mrs
    )) +
    geom_col(width = 0.5) +
    geom_text(
      aes(label = pct_label),
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
        "0: No symptoms",
        "1: No significant disability",
        "2: Slight disability",
        "3: Moderate disability",
        "4: Moderately severe disability",
        "5: Severe disability",
        "6: Dead"
      ),
      values = c(
        "#476170",
        "#7190a8",
        "#8bb4d0",
        "#b6dde5",
        "#eec6a2",
        "#d98174",
        "#ce4950"
      )
    ) +
    labs(
      x = NULL,
      y = NULL,
      fill = "mRS at 90 days"
    ) +
    theme_minimal()
}


get_outcome_labels <- function(variable_name) {
  if (grepl("mrs", variable_name)) {
    return(c(
      "0" = "0: No symptoms",
      "1" = "1: No significant disability",
      "2" = "2: Slight disability",
      "3" = "3: Moderate disability",
      "4" = "4: Moderately severe disability",
      "5" = "5: Severe disability",
      "6" = "6: Dead"
    ))
  } else if (grepl("euro", variable_name)) {
    # EuroQOL typically uses 1-5 for levels of problems
    return(c(
      "1" = "1: No problems",
      "2" = "2: Some problems",
      "3" = "3: Extreme problems"
    ))
  }
}


library(marginaleffects)
library(tidyverse)

make_mrs_evidence_plot <- function(model) {
  # 1. Calculate the difference: Right minus Left
  # This isolates the effect of laterality
  diff_data <- avg_comparisons(
    model,
    variables = list(ich_laterality = "reference"),
    by = "group",
    comparison = "difference"
  ) |>
    as_tibble() |>
    mutate(
      mrs_label = case_when(
        group == 0 ~ "0",
        group == 1 ~ "1",
        group == 2 ~ "2",
        group == 3 ~ "3",
        group == 4 ~ "4",
        group == 5 ~ "5",
        group == 6 ~ "6"
      ),
      mrs_label = fct_rev(mrs_label)
    )

  # 2. Plot the differences with Credible Intervals
  ggplot(diff_data, aes(x = estimate, y = mrs_label)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_pointrange(
      aes(xmin = conf.low, xmax = conf.high),
      color = "#476170",
      size = 0.8
    ) + # Using your 'Right' fill color
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = "Laterality Effect (Right - Left)",
      x = "Probability Difference",
      y = "mRS Level"
    ) +
    theme_minimal() # Using your project theme
}
