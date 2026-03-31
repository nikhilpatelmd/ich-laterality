# R/mrs_figures.R
#
# Two-panel mRS outcome figure:
#   Panel A — Adjusted Grotta bars: posterior marginal mean probability for
#              each mRS category, by ICH laterality
#   Panel B — Posterior uncertainty: overlapping density curves (one per
#              laterality group) showing the full posterior distribution of
#              predicted probabilities, faceted by mRS category; vertical
#              segments mark each group's median, clipped at the density peak
#
# Entry point: make_mrs_figure(model)
# Sub-functions are exported for independent use (e.g., supplemental figures).

library(marginaleffects)
library(tidyverse)
library(patchwork)
library(ggtext) # geom_richtext() for Panel A label backgrounds

# ── Shared constants ───────────────────────────────────────────────────────────

mrs_labels <- c(
  "0" = "mRS 0: No symptoms",
  "1" = "mRS 1: No significant disability",
  "2" = "mRS 2: Slight disability",
  "3" = "mRS 3: Moderate disability",
  "4" = "mRS 4: Moderately severe disability",
  "5" = "mRS 5: Severe disability",
  "6" = "mRS 6: Dead"
)

# Short labels for Panel B strip titles — full descriptions are redundant once
# the reader has read Panel A's legend
mrs_labels_short <- c(
  "0" = "mRS 0",
  "1" = "mRS 1",
  "2" = "mRS 2",
  "3" = "mRS 3",
  "4" = "mRS 4",
  "5" = "mRS 5",
  "6" = "mRS 6"
)

# Okabe-Ito palette — consistent with laterality colouring used elsewhere
laterality_colors <- c("Left" = "#0072B2", "Right" = "#D55E00")

# Base font size applied to both panels via theme_minimal(base_size = BASE_PT).
# Scaling through base_size (rather than overriding each element individually)
# keeps proportions consistent — strip text, legend text, tick labels, and axis
# titles all grow together at the same ratio.
BASE_PT <- 14


# ── Helpers ───────────────────────────────────────────────────────────────────

label_mrs <- function(group_col) {
  fct_rev(factor(mrs_labels[as.character(group_col)], levels = mrs_labels))
}

label_mrs_fwd_short <- function(group_col) {
  factor(mrs_labels_short[as.character(group_col)], levels = mrs_labels_short)
}


# ── Panel A: Adjusted Grotta bars ─────────────────────────────────────────────

make_grotta_bars <- function(model) {
  avg_predictions(model, by = "ich_laterality") |>
    as_tibble() |>
    rename(mrs_90 = group, prob = estimate) |>
    mutate(
      mrs = label_mrs(mrs_90),
      prob_label = if_else(
        prob >= 0.03,
        scales::percent(prob, accuracy = 0.1),
        ""
      )
    ) |>
    ggplot(aes(x = ich_laterality, y = prob, fill = mrs)) +
    geom_col(width = 0.5) +
    geom_richtext(
      aes(label = prob_label),
      position = position_stack(vjust = 0.5),
      fill = "white",
      color = "grey20",
      label.color = NA,
      fontface = "bold",
      size = (BASE_PT / 0.9) / 2.835, # convert pt → ggplot size units
      family = "Arial",
      label.padding = unit(c(0.15, 0.2, 0.15, 0.2), "lines")
    ) +
    coord_flip() +
    scale_y_continuous(
      labels = scales::percent,
      limits = c(0, 1),
      expand = c(0, 0)
    ) +
    scale_fill_viridis_d(option = "mako", direction = 1) +
    labs(
      x = NULL,
      y = "Predicted probability at 90 days",
      fill = "mRS at 90 days"
    ) +
    theme_minimal(base_size = BASE_PT, base_family = "Arial") +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      # Add vertical breathing room between the tick labels and the axis title
      axis.title.x = element_text(margin = margin(t = 8))
    )
}


# ── Panel B: Posterior uncertainty (density + clipped median segments) ─────────
# Overlapping densities show the shape of posterior uncertainty; segments at
# each group's median anchor the eye to the central estimate and are clipped
# at the curve peak via approx() interpolation of the KDE.
#
# X-axis design rationale:
#   - breaks_pretty(n = 3) forces exactly 3 tick marks per facet regardless of
#     the range width, preventing the default algorithm from adding extra ticks
#     on very narrow ranges (a few percentage points)
#   - accuracy = 1 (whole-number percents) is appropriate here because the tick
#     labels serve only to orient the reader to the approximate probability range
#     for each category; they are not precision readouts, and the extra decimal
#     makes each label physically wider and more prone to collision

make_mrs_uncertainty <- function(model) {
  draws <- avg_predictions(model, by = "ich_laterality") |>
    posterior_draws() |>
    as_tibble() |>
    rename(mrs_90 = group) |>
    mutate(mrs = label_mrs_fwd_short(mrs_90))

  median_draws <- draws |>
    group_by(ich_laterality, mrs) |>
    summarise(
      median_prob = median(draw),
      density_at_median = {
        d <- density(draw)
        approx(d$x, d$y, xout = median(draw))$y
      },
      .groups = "drop"
    )

  ggplot(draws, aes(x = draw, fill = ich_laterality, color = ich_laterality)) +
    geom_segment(
      data = median_draws,
      aes(
        x = median_prob,
        xend = median_prob,
        y = 0,
        yend = density_at_median,
        color = ich_laterality
      ),
      linewidth = 0.7,
      linetype = "dashed",
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    geom_density(alpha = 0.35, linewidth = 0.6) +
    facet_wrap(~mrs, nrow = 1, scales = "free_x") +
    scale_x_continuous(
      # 3 breaks per facet: enough to read the range, not so many that
      # labels collide on the narrow (~4 pp) windows most facets occupy
      breaks = scales::breaks_pretty(n = 3),
      # Whole-number percents: these are orientation ticks, not precise
      # readouts — dropping the decimal makes each label narrower and
      # reduces collision risk at 30° rotation
      labels = scales::percent_format(accuracy = 1)
    ) +
    scale_fill_manual(values = laterality_colors) +
    scale_color_manual(values = laterality_colors) +
    guides(
      fill = guide_legend(
        title = "ICH laterality",
        override.aes = list(linetype = 0, linewidth = 0)
      ),
      color = guide_legend(
        title = "ICH laterality",
        override.aes = list(linetype = 0, linewidth = 0)
      )
    ) +
    labs(
      x = "Predicted probability at 90 days",
      y = "Posterior density",
      fill = "ICH laterality",
      color = "ICH laterality"
    ) +
    theme_minimal(base_size = BASE_PT, base_family = "Arial") +
    theme(
      legend.position = "right",
      strip.text = element_text(face = "bold", family = "Arial"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 30, hjust = 1),
      # Breathing room between rotated tick labels and the axis title below
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10))
    )
}


# ── Combined two-panel figure ─────────────────────────────────────────────────

make_mrs_figure <- function(model) {
  panel_a <- make_grotta_bars(model)
  panel_b <- make_mrs_uncertainty(model)

  (panel_a / panel_b) +
    plot_annotation(tag_levels = "A") +
    plot_layout(heights = c(1, 1), guides = "keep")
}
