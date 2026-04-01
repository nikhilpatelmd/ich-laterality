# R/euro_figures.R
#
# Two-panel EuroQOL (EQ-5D-3L) outcome figure — structural analogue of
# mrs_figures.R, adapted for the five EQ-5D dimensions. Each dimension is a
# 3-level cumulative logit model, so Panel A shows adjusted stacked bars over
# three categories and Panel B shows posterior density curves faceted by those
# three categories.
#
# Entry point: make_euro_figure(model, dimension)
#   dimension — one of "mobility", "selfcare", "usual", "pain", "anxiety"
#
# Sub-functions are exported for independent use (e.g., supplemental figures).



# ── Shared constants ───────────────────────────────────────────────────────────

# Full labels for Panel A legend, keyed by dimension then factor level.
# EQ-5D-3L levels are universally coded 1/2/3; the wording of level 3 ("unable
# to" vs. "confined to bed" vs. "extreme") is dimension-specific.
euro_labels <- list(
  mobility = c(
    "1" = "Level 1: No problems walking about",
    "2" = "Level 2: Some problems walking about",
    "3" = "Level 3: Confined to bed"
  ),
  selfcare = c(
    "1" = "Level 1: No problems with self-care",
    "2" = "Level 2: Some problems washing or dressing",
    "3" = "Level 3: Unable to wash or dress"
  ),
  usual = c(
    "1" = "Level 1: No problems with usual activities",
    "2" = "Level 2: Some problems with usual activities",
    "3" = "Level 3: Unable to perform usual activities"
  ),
  pain = c(
    "1" = "Level 1: No pain or discomfort",
    "2" = "Level 2: Moderate pain or discomfort",
    "3" = "Level 3: Extreme pain or discomfort"
  ),
  anxiety = c(
    "1" = "Level 1: Not anxious or depressed",
    "2" = "Level 2: Moderately anxious or depressed",
    "3" = "Level 3: Extremely anxious or depressed"
  )
)

# Short labels for Panel B strip titles — full descriptions are redundant once
# the reader has seen Panel A's legend
euro_labels_short <- c(
  "1" = "Level 1",
  "2" = "Level 2",
  "3" = "Level 3"
)

# Human-readable dimension titles for legend headings and axis annotations.
# These should exactly match the clinical wording used in the manuscript.
euro_dimension_titles <- c(
  mobility = "Mobility",
  selfcare = "Self-Care",
  usual = "Usual Activities",
  pain = "Pain / Discomfort",
  anxiety = "Anxiety / Depression"
)

# Okabe-Ito palette — consistent with laterality colouring used elsewhere
laterality_colors <- c("Left" = "#0072B2", "Right" = "#D55E00")

# Base font size applied to both panels via theme_minimal(base_size = BASE_PT).
# All text elements (strip labels, tick labels, legend, axis titles) scale
# together at the same ratio — no element-by-element overrides needed.
BASE_PT <- 14


# ── Helpers ───────────────────────────────────────────────────────────────────

# label_euro() maps the integer factor level to its full description, then
# *reverses* level order so that the most favourable category (Level 1) sits
# at the top of the stacked bar — matching the mRS convention where mRS 0 is
# at the top.
label_euro <- function(group_col, dimension) {
  lbls <- euro_labels[[dimension]]
  fct_rev(factor(lbls[as.character(group_col)], levels = lbls))
}

# label_euro_fwd_short() returns levels in forward order (1 → 2 → 3) using
# short labels for Panel B facet strips, so strips read left-to-right from
# most to least favourable.
label_euro_fwd_short <- function(group_col) {
  factor(
    euro_labels_short[as.character(group_col)],
    levels = euro_labels_short
  )
}


# ── Panel A: Adjusted stacked bars ────────────────────────────────────────────

make_euro_bars <- function(model, dimension) {
  dim_title <- euro_dimension_titles[[dimension]]

  avg_predictions(model, by = "ich_laterality") |>
    as_tibble() |>
    rename(euro_level = group, prob = estimate) |>
    # avg_predictions() on a brmsfit_multiple preserves the outcome variable's
    # factor structure, which includes a missing category when the raw data has
    # missing outcome values. Those rows carry no probability mass and have no
    # clinical meaning — missingness is handled by MICE upstream, not here.
    # brms encodes this as the *string* "NA" (not a true R NA), so is.na()
    # alone won't catch it; we need both guards.
    filter(!is.na(euro_level), euro_level != "NA") |>
    mutate(
      level = label_euro(euro_level, dimension),
      prob_label = if_else(
        prob >= 0.03,
        scales::percent(prob, accuracy = 0.1),
        ""
      )
    ) |>
    ggplot(aes(x = ich_laterality, y = prob, fill = level)) +
    geom_col(width = 0.5) +
    geom_richtext(
      aes(label = prob_label),
      position = position_stack(vjust = 0.5),
      fill = "white",
      color = "grey20",
      label.color = NA,
      fontface = "bold",
      size = (BASE_PT / 0.75) / 2.835, # convert pt → ggplot size units
      family = "Arial",
      label.padding = unit(c(0.15, 0.2, 0.15, 0.2), "lines")
    ) +
    coord_flip() +
    scale_y_continuous(
      labels = scales::percent,
      limits = c(0, 1),
      expand = c(0, 0)
    ) +
    # With only 3 levels, mako gives three perceptually distinct hues that
    # still read as a natural progression from favourable to unfavourable.
    scale_fill_viridis_d(option = "mako", direction = 1) +
    # reverse = TRUE re-orders the legend keys so Level 1 (most favourable)
    # sits at the top, matching the reading direction of the bars. The factor
    # itself stays fct_rev() so the bar stacking order is unaffected.
    guides(fill = guide_legend(reverse = TRUE)) +
    labs(
      x = NULL,
      y = "Predicted probability at 90 days",
      fill = glue::glue("EQ-5D-3L\n{dim_title}")
    ) +
    theme_minimal(base_size = BASE_PT, base_family = "Arial") +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      axis.title.x = element_text(margin = margin(t = 8))
    )
}


# ── Panel B: Posterior uncertainty (density + clipped median segments) ─────────
# Design rationale is identical to mrs_figures.R: overlapping densities reveal
# the shape of posterior uncertainty; dashed segments at each group's median
# anchor the eye to the central estimate and are clipped at the curve peak via
# approx() interpolation of the KDE.
#
# With only 3 facets (vs. 7 for mRS), the panel is naturally wider per facet,
# so axis text rarely collides — but we still rotate 30° for consistency with
# the mRS figure when both appear together in the supplement.

make_euro_uncertainty <- function(model, dimension) {
  draws <- avg_predictions(model, by = "ich_laterality") |>
    posterior_draws() |>
    as_tibble() |>
    rename(euro_level = group) |>
    # Same issue as in make_euro_bars(): brms encodes the missing category as
    # the string "NA", not a true R NA, so both guards are needed here too.
    filter(!is.na(euro_level), euro_level != "NA") |>
    mutate(level = label_euro_fwd_short(euro_level))

  median_draws <- draws |>
    group_by(ich_laterality, level) |>
    summarise(
      median_prob = median(draw),
      # Interpolate KDE at the median to know where to clip the segment top
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
    # nrow = 1 keeps the three facets side-by-side, matching the mRS layout
    facet_wrap(~level, nrow = 1, scales = "free_x") +
    scale_x_continuous(
      # 3 breaks per facet: enough to read the range without collision.
      breaks = scales::breaks_pretty(n = 3),
      # Whole-number percents: orientation ticks, not precision readouts.
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
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10))
    )
}


# ── Combined two-panel figure ─────────────────────────────────────────────────

make_euro_figure <- function(model, dimension) {
  # Validate dimension early so the error message names the allowed values
  # rather than crashing inside label_euro() with a cryptic subscript error.
  dimension <- match.arg(
    dimension,
    choices = names(euro_labels)
  )

  panel_a <- make_euro_bars(model, dimension)
  panel_b <- make_euro_uncertainty(model, dimension)

  (panel_a / panel_b) +
    plot_annotation(tag_levels = "A") +
    plot_layout(heights = c(1, 1), guides = "keep")
}
