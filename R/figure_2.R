# R/figure2.R
#
# make_posterior_prob_figure()
#   Generates a two-panel posterior probability figure for any binary outcome
#   model from the ICH laterality study.
#
#   Panel A: Overlapping posterior density curves of the population-averaged
#            predicted probability for Left vs. Right hemisphere patients.
#   Panel B: Posterior density of the absolute probability difference
#            (Right - Left), with a reference line at zero.
#
# format_posterior_prob()
#   Helper for displaying posterior probabilities from finite MCMC samples,
#   handling the 0% / 100% ceiling problem.
#
# Dependencies: marginaleffects, ggplot2, ggtext, patchwork, dplyr,
#               tidyr, scales, glue, stringr

make_posterior_prob_figure <- function(
  model, # fitted brms model (brmsfit or brmsfit_multiple)
  outcome_label, # clinical label used in axis titles, e.g. "Neurosurgical Intervention"
  covariate_caption, # sentence(s) describing the adjustment set, appended to caption

  laterality_var = "ich_laterality",
  ref_level = "Left",
  contrast_level = "Right",

  # x_limits in percentage points — tune per outcome by inspecting
  # avg_predictions() before committing to a range
  x_limits = c(0, 20),

  # base_pt drives all text sizing; geom sizes are derived via base_mm
  # so the figure scales consistently when base_pt changes
  base_pt = 14,

  caption_width = 110,
  palette = c(Left = "#0072B2", Right = "#D55E00"),
  diff_fill = "#7B2D8B"
) {
  # ── Model extraction ───────────────────────────────────────────────────────

  # ggplot2 theme uses points; geom size arguments use mm.
  # Conversion: 1 pt = 1/72 inch; 1 mm = 1/25.4 inch → ~2.835 pts per mm.
  base_mm <- base_pt / 2.835

  # ── Posterior draws ────────────────────────────────────────────────────────
  # avg_predictions(by = laterality_var) computes the population-averaged
  # predicted probability for each group at each posterior draw — the
  # epidemiological "marginal" probability that reviewers ask for.
  # posterior_draws() unpacks to one row per (drawid x laterality level),
  # with the predicted probability in the `draw` column.
  pred_draws <- avg_predictions(model, by = laterality_var) |>
    posterior_draws()

  # ── Group-level summary for Panel A annotations ───────────────────────────
  # estimate, conf.low, conf.high are repeated constants on pred_draws —
  # the same marginal summary attached to every row for convenience.
  # distinct() collapses to one row per group before converting to percentages.
  pred_summary <- pred_draws |>
    distinct(.data[[laterality_var]], estimate, conf.low, conf.high) |>
    mutate(across(where(is.numeric), ~ .x * 100))

  # Compute the density peak dynamically so annotation y-positions adapt
  # to the actual distribution shape across different outcomes.
  # Rare outcomes (e.g. neurosurgery ~5%) produce tall narrow peaks;
  # common ones (e.g. DNR ~50%) produce shorter wider ones.
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
        density_peak * 0.55, # ref level annotation sits higher
        density_peak * 0.32 # contrast level annotation sits lower
      )
    )

  # ── Within-draw differences for Panel B ───────────────────────────────────
  # Pivoting wide before subtracting computes the contrast within each draw,
  # propagating joint posterior uncertainty correctly. Subtracting posterior
  # summaries instead would lose the covariance between the two group estimates.
  diff_draws <- pred_draws |>
    pivot_wider(
      id_cols = drawid,
      names_from = .data[[laterality_var]],
      values_from = draw
    ) |>
    mutate(difference = .data[[contrast_level]] - .data[[ref_level]])

  # prob_right_greater is kept on the 0-1 scale so format_posterior_prob()
  # can consume it directly. Only the distance metrics are converted to
  # percentage points, avoiding a confusing divide-by-100 later.
  diff_summary <- diff_draws |>
    summarize(
      median = median(difference),
      prob_right_greater = sum(difference > 0) / n(),
      lower_ci = quantile(difference, 0.025),
      upper_ci = quantile(difference, 0.975)
    ) |>
    mutate(across(c(median, lower_ci, upper_ci), ~ .x * 100))

  # ── Shared y-axis ceiling ──────────────────────────────────────────────────
  # Both panels share the same y-axis limit so distributional widths are
  # directly comparable. The 10% headroom prevents the peak from touching
  # the top of the panel.
  y_ceiling <- density_peak * 1.1

  # ── Panel A: absolute probabilities ───────────────────────────────────────
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
        # Annotation starts at 52% of the x range — sits in the whitespace
        # to the right of both distributions, adapts to any x_limits value
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

  # ── Panel B: difference distribution ──────────────────────────────────────
  p_diff <- diff_draws |>
    mutate(difference_pct = difference * 100) |>
    ggplot(aes(x = difference_pct)) +
    geom_density(alpha = 0.6, fill = diff_fill) +
    coord_cartesian(ylim = c(0, y_ceiling)) +
    # Reference line at zero: the "no difference" null benchmark.
    # Retained even when P(Right > Left) ~ 100% because it gives the reader
    # a visual anchor for how far the posterior sits from null.
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
      y = NULL # shared meaning with Panel A; label removed to reduce redundancy
    ) +
    scale_x_continuous(
      labels = label_number(suffix = " pp", accuracy = 0.1),
      limits = x_limits,
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    theme_minimal(base_family = "Arial", base_size = base_pt) +
    theme(legend.position = "none")

  # ── Combine panels ─────────────────────────────────────────────────────────
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

  p_abs /
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
}


# ── format_posterior_prob() ────────────────────────────────────────────────────
# With N posterior draws the finest probability resolution is 1/N. For 40,000
# draws that is 0.0025%, so claiming exactly 0% or 100% overstates certainty.
# This function reports "> 99.9%" and "< 0.1%" at the boundaries instead.
# Used consistently across all figures and tables in the pipeline.
#
# p      : probability on the 0-1 scale (NOT pre-multiplied by 100)
# digits : decimal places for display (default 1)

format_posterior_prob <- function(p, digits = 1) {
  dplyr::case_when(
    p >= 0.999 ~ "> 99.9%",
    p <= 0.001 ~ "< 0.1%",
    TRUE ~ paste0(round(p * 100, digits), "%")
  )
}
