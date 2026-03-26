# R/make_posterior_prob_figure.R
#
# Generates a two-panel posterior probability figure for any binary outcome
# model from the ICH laterality study.
#
# Panel A: Overlapping posterior density curves of the population-averaged
#          predicted probability for Left vs. Right hemisphere patients.
# Panel B: Posterior density of the absolute probability difference
#          (Right - Left), with a reference line at zero.
#
# Dependencies: marginaleffects, ggplot2, ggtext, patchwork, dplyr,
#               tidyr, scales, glue, stringr

make_posterior_prob_figure <- function(
  # The fitted brms model object (brmsfit or brmsfit_multiple)
  model,

  # Short clinical label for the outcome, used in axis titles and caption.
  # e.g., "Neurosurgical Intervention", "EVD Placement", "Tracheostomy"
  outcome_label,

  # One or two sentences describing the covariates in the model, appended
  # to the standard caption boilerplate. This keeps the caption self-contained
  # for each outcome without hard-coding covariate lists inside the function.
  covariate_caption,

  # Column name identifying the laterality grouping variable
  laterality_var = "ich_laterality",

  # Reference and contrast levels — Right - Left is the natural direction
  # since the model coefficient for Right is positive for most outcomes
  ref_level = "Left",
  contrast_level = "Right",

  # X-axis limits in percentage points. The default c(0, 20) works well for
  # outcomes like neurosurgery; you may want c(0, 50) for more common outcomes
  # like DNR or comfort care, or c(-5, 15) for very rare ones.
  x_limits = c(0, 20),

  # Base font size in points. All other sizes (geom text, caption) are
  # derived from this so the figure scales consistently.
  base_pt = 14,

  # Character width for caption line wrapping
  caption_width = 110,

  # Named color vector for the two hemisphere groups
  palette = c(Left = "#0072B2", Right = "#D55E00"),

  # Fill color for the difference distribution in Panel B
  diff_fill = "#7B2D8B"
) {
  # ── Step 0: Extract brmsfit from brmsfit_multiple ─────────────────────────
  # All models in this pipeline are brmsfit_multiple objects produced by
  # brm_multiple() over MICE-imputed datasets. avg_predictions() requires
  # a single brmsfit object, so we extract the first imputation here.
  #
  # Importantly, this does NOT mean we are discarding posterior draws —
  # each imputation still contains the full set of 40,000 post-warmup draws
  # (20 chains x 2,000 iterations each). What we are not capturing is the
  # between-imputation variance: the additional uncertainty from "which
  # imputed dataset was the correct one." For a well-specified MICE run with
  # modest missingness, this component is typically small relative to the
  # within-imputation (posterior) variance, but it is worth noting in the
  # methods.
  #
  # We use inherits() rather than always doing [[1]] unconditionally so that
  # the function also works correctly on a plain brmsfit object (e.g., a
  # complete-case sensitivity model) without erroring on a non-list.
  if (inherits(model, "brmsfit_multiple") || is.list(model)) {
    model <- model[[1]]
  }

  # Derived sizing constant: converts base_pt (points) to mm for geom sizing.
  # ggplot2 theme arguments use points; geom size arguments use mm.
  # Conversion: 1 pt = 1/72 inch; 1 mm = 1/25.4 inch → ~2.835 pts per mm.
  base_mm <- base_pt / 2.835

  # ── Step 1: Extract posterior draws ────────────────────────────────────────
  # avg_predictions(by = laterality_var) computes the population-averaged
  # predicted probability for each group at each posterior draw, giving us
  # 2 x N_draws rows. posterior_draws() unpacks these into long format with
  # one row per (drawid x laterality level), with the predicted probability
  # stored in the `draw` column.
  pred_draws <- avg_predictions(model, by = laterality_var) |>
    posterior_draws()

  # ── Step 2: Group-level summary for Panel A annotations ───────────────────
  # estimate, conf.low, and conf.high are repeated constants on pred_draws
  # (the same marginal summary attached to every row for convenience).
  # distinct() collapses to one row per group before converting to percentages.
  pred_summary <- pred_draws |>
    distinct(.data[[laterality_var]], estimate, conf.low, conf.high) |>
    mutate(across(where(is.numeric), ~ .x * 100))

  # Dynamically position annotations relative to the actual density peak.
  # This prevents crowding for narrow distributions (rare outcomes like
  # neurosurgery) and avoids annotations floating too high for common ones
  # (like DNR). Left annotation sits higher, Right lower, to avoid overlap.
  density_peak <- pred_draws |>
    mutate(draw_pct = draw * 100) |>
    group_by(.data[[laterality_var]]) |>
    summarise(peak = max(density(draw_pct)$y), .groups = "drop") |>
    pull(peak) |>
    max()

  pred_summary <- pred_summary |>
    mutate(
      y_pos = if_else(
        .data[[laterality_var]] == ref_level,
        density_peak * 0.55,
        density_peak * 0.32
      )
    )

  # ── Step 3: Within-draw differences for Panel B ───────────────────────────
  # Pivoting wide before subtracting ensures the contrast is computed
  # within each draw, correctly propagating joint posterior uncertainty.
  # Subtracting posterior summaries instead would lose the covariance
  # structure between the two group estimates.
  diff_draws <- pred_draws |>
    pivot_wider(
      id_cols = drawid,
      names_from = .data[[laterality_var]],
      values_from = draw
    ) |>
    mutate(difference = .data[[contrast_level]] - .data[[ref_level]])

  # Keep prob_right_greater on the 0-1 scale so format_posterior_prob()
  # can consume it directly without a divide-by-100 step.
  # Only the distance metrics (median, CrI bounds) get converted to
  # percentage points.
  diff_summary <- diff_draws |>
    summarize(
      median = median(difference),
      prob_right_greater = sum(difference > 0) / n(),
      lower_ci = quantile(difference, 0.025),
      upper_ci = quantile(difference, 0.975)
    ) |>
    mutate(across(c(median, lower_ci, upper_ci), ~ .x * 100))

  # ── Step 4: Shared y-axis ceiling ─────────────────────────────────────────
  # Both panels use the same y-axis limit so the reader can compare
  # distributional width without being misled by different vertical scales.
  # We add 10% headroom above the tallest peak for visual breathing room.
  y_ceiling <- density_peak * 1.1

  # ── Step 5: Panel A — absolute probabilities ──────────────────────────────
  p_abs <- pred_draws |>
    mutate(draw_pct = draw * 100) |>
    ggplot(aes(x = draw_pct, fill = .data[[laterality_var]])) +
    geom_density(alpha = 0.6) +
    coord_cartesian(ylim = c(0, y_ceiling)) +
    scale_fill_manual(values = palette) +
    scale_color_manual(values = palette) +
    geom_vline(
      data = pred_summary,
      aes(xintercept = estimate, color = .data[[laterality_var]]),
      linetype = "dotted",
      linewidth = 1.5
    ) +
    geom_richtext(
      data = pred_summary,
      aes(
        # Start annotation at 52% of the x range to sit in the whitespace
        # to the right of both distributions. Uses range arithmetic rather
        # than a hardcoded value so it adapts to different x_limits.
        x = x_limits[1] + (x_limits[2] - x_limits[1]) * 0.52,
        y = y_pos,
        label = glue(
          "**{.data[[laterality_var]]} Hemisphere: {round(estimate, 2)}%**<br>",
          "95% CrI ({round(conf.low, 2)} \u2013 {round(conf.high, 2)}%)"
        )
      ),
      fill = NA,
      label.color = NA,
      size = base_mm * 0.85,
      family = "Arial",
      hjust = 0
    ) +
    labs(
      x = paste0("P(", outcome_label, ")"),
      y = "Probability Density",
      fill = "ICH Laterality"
    ) +
    scale_x_continuous(
      labels = label_number(suffix = "%", accuracy = 1),
      limits = x_limits,
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    theme_minimal(base_family = "Arial", base_size = base_pt) +
    theme(legend.position = c(0.12, 0.85)) +
    guides(color = "none")

  # ── Step 6: Panel B — difference distribution ─────────────────────────────
  p_diff <- diff_draws |>
    mutate(difference_pct = difference * 100) |>
    ggplot(aes(x = difference_pct)) +
    geom_density(alpha = 0.6, fill = diff_fill) +
    coord_cartesian(ylim = c(0, y_ceiling)) +
    # Grey reference line at zero: the "no difference" null benchmark.
    # Even when P(Right > Left) is near 100%, this line is important because
    # it gives the reader a visual anchor for how far the posterior sits
    # from null — which is more informative than the probability alone.
    geom_vline(
      xintercept = 0,
      linetype = "dotted",
      linewidth = 1.5,
      color = "grey50"
    ) +
    geom_vline(
      data = diff_summary,
      aes(xintercept = median),
      linetype = "dotted",
      linewidth = 1.5,
      color = diff_fill
    ) +
    geom_richtext(
      data = diff_summary,
      aes(
        x = x_limits[1] + (x_limits[2] - x_limits[1]) * 0.52,
        y = y_ceiling * 0.55,
        label = glue(
          "**Median Difference ({contrast_level} \u2212 {ref_level})**:",
          " {round(median, 2)} pp<br>",
          "95% CrI ({round(lower_ci, 2)} \u2013 {round(upper_ci, 2)} pp)<br><br>",
          "**P({contrast_level} > {ref_level})**:",
          " {format_posterior_prob(prob_right_greater)}"
        )
      ),
      fill = NA,
      label.color = NA,
      size = base_mm * 0.85,
      family = "Arial",
      hjust = 0
    ) +
    labs(
      x = paste0(
        "Absolute Probability Difference (",
        contrast_level,
        " \u2212 ",
        ref_level,
        "), pp"
      ),
      y = NULL # shared meaning with Panel A; removing reduces redundancy
    ) +
    scale_x_continuous(
      labels = label_number(suffix = " pp", accuracy = 0.1),
      limits = x_limits,
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    theme_minimal(base_family = "Arial", base_size = base_pt) +
    theme(legend.position = "none")

  # ── Step 7: Combine panels with patchwork ─────────────────────────────────
  caption_text <- str_wrap(
    paste0(
      "Posterior distributions of covariate-adjusted average predicted ",
      "probabilities of ",
      tolower(outcome_label),
      " by ICH hemisphere. ",
      "Estimates reflect the population-averaged probability across the ",
      "observed covariate distribution. ",
      covariate_caption,
      " ",
      "Dotted lines indicate posterior medians. Shaded regions represent ",
      "the full posterior distribution. ",
      "pp = percentage points. CrI = credible interval."
    ),
    width = caption_width
  )

  combined <- p_abs /
    p_diff +
    plot_layout(heights = c(1.5, 1)) +
    plot_annotation(
      tag_levels = "A",
      caption = caption_text,
      theme = theme(
        plot.tag = element_text(
          face = "bold",
          size = base_pt + 2,
          family = "Arial"
        ),
        plot.caption = element_text(
          size = base_pt - 2,
          color = "grey40",
          hjust = 0,
          family = "Arial"
        )
      )
    )

  return(combined)
}


# ── format_posterior_prob() ───────────────────────────────────────────────────
# Helper for displaying posterior probabilities from finite MCMC samples.
#
# The core problem: with N posterior draws, the finest probability resolution
# is 1/N. For 40,000 draws, that is 0.0025%. Reporting exactly "100%" implies
# infinite certainty — no finite sample can establish this. Instead we report
# "> 99.9%" at the ceiling and "< 0.1%" at the floor. This convention is used
# consistently across all figures and tables in the pipeline.
#
# Arguments:
#   p      : probability on the 0-1 scale (NOT pre-multiplied by 100)
#   digits : decimal places for display (default 1)
#
# Returns a formatted string like "97.3%", "> 99.9%", or "< 0.1%"

format_posterior_prob <- function(p, digits = 1) {
  dplyr::case_when(
    p >= 0.999 ~ "> 99.9%",
    p <= 0.001 ~ "< 0.1%",
    TRUE ~ paste0(round(p * 100, digits), "%")
  )
}
