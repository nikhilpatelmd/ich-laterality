ggplot(mutate(spread_draws(m_prior_informative_neurosurgery, b_ich_lateralityRight),
        ich_right_or = exp(b_ich_lateralityRight)
), aes(ich_right_or)) +
        geom_density(color = "#422f05", fill = "#E69F00") +
        scale_x_continuous(breaks = seq(
                0,
                3, 0.25
        )) +
        labs(
                title = "Odds Ratio - Right Hemisphere",
                y = NULL, fill = NULL
        ) +
        geom_vline(
                xintercept = 1, linetype = "dashed",
                color = "#999999", linewidth = 1
        ) +
        theme_ich() +
        theme(legend.position = "none")
