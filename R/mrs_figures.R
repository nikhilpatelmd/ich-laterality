tar_load(m_posterior_neutral_mrs_90_canonical)
tar_load(ich_aggressive)

adjusted_mrs_function <- function(m_posterior_neutral_mrs_90_canonical) {

  pred <- avg_predictions(
    m_posterior_neutral_mrs_90_canonical,
    by = "ich_laterality"
  )
  bob
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
      "0: No symptoms",
      "1: No significant disability",
      "2: Slight disability",
      "3: Moderate disability",
      "4: Moderately severe disability",
      "5: Severe disability",
      "6: Dead"
    ),
    values = c("#476170", "#7190a8", "#8bb4d0", "#b6dde5", "#eec6a2", "#d98174", "#ce4950")
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = "mRS at 90 days"
  ) +
  theme_ich() +
    
    # Add dotted lines using the segment_data (without aes())
   geom_segment(
    data = segment_data,
    x = 1.5, y = 0.1, xend = 1.5, yend = 0.9,  # Directly specify coordinates
    color = "black",
    linetype = "dotted",
    inherit.aes = FALSE  # Add this line
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
          "0: No symptoms",
          "1: No significant disability",
          "2: Slight disability",
          "3: Moderate disability",
          "4: Moderately severe disability",
          "5: Severe disability",
          "6: Dead"
        ),
        values = c("#476170", "#7190a8", "#8bb4d0", "#b6dde5", "#eec6a2", "#d98174", "#ce4950")
      ) +
      labs(
        x = NULL,
        y = NULL,
        fill = "mRS at 90 days"
      ) +
      theme_ich() 
  }