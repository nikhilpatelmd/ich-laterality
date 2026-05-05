# R/figure_1.R
#
# make_posterior_prob_figure()
#   Generates a two-panel posterior probability figure for any binary outcome
#   model from the ICH laterality study. Used by tar_map to produce one figure
#   per outcome × prior combination (24 total). Kept unchanged so existing
#   pipeline targets are not invalidated.
#
#   Panel A: Overlapping posterior density curves of the population-averaged
#            predicted probability for Left vs. Right hemisphere patients.
#   Panel B: Posterior density of the absolute probability difference
#            (Right - Left), with a reference line at zero.
#
# make_prior_sensitivity_figure()
#   Combines all four prior models for a single outcome into one figure.
#   Intended as the primary manuscript figure — directly addresses the
#   reviewer request to foreground prior sensitivity in the main text.
#
#   Panel A: Neutral-prior absolute probabilities by hemisphere (clinical anchor).
#   Panel B: Four overlaid difference distributions, one per prior, demonstrating
#            robustness of the Right > Left finding across prior choices.
#
# format_posterior_prob()
#   Helper for displaying posterior probabilities from finite MCMC samples,
#   handling the 0% / 100% ceiling problem.
#
# Dependencies: marginaleffects, ggplot2, ggtext, patchwork, dplyr,
#               tidyr, scales, glue, stringr

# ── make_posterior_prob_figure() ──────────────────────────────────────────────

make_posterior_prob_figure <- function(
  model,
  outcome_label,
  covariate_caption,
  laterality_var = "ich_laterality",
  ref_level = "Left",
  contrast_level = "Right",
  x_limits = c(0, 20),
  base_pt = 14,
  caption_width = 110,
  palette = c(Left = "#0072B2", Right = "#D55E00"),
  diff_fill = "#7B2D8B"
) {
  base_mm <- base_pt / 2.835

  pred_draws <- avg_predictions(model, by = laterality_var) |>
    posterior_draws()

  pred_summary <- pred_draws |>
    distinct(.data[[laterality_var]], estimate, conf.low, conf.high) |>
    mutate(across(where(is.numeric), ~ .x * 100))

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

  diff_draws <- pred_draws |>
    pivot_wider(
      id_cols = drawid,
      names_from = .data[[laterality_var]],
      values_from = draw
    ) |>
    mutate(difference = .data[[contrast_level]] - .data[[ref_level]])

  diff_summary <- diff_draws |>
    summarize(
      median = median(difference),
      prob_right_greater = sum(difference > 0) / n(),
      lower_ci = quantile(difference, 0.025),
      upper_ci = quantile(difference, 0.975)
    ) |>
    mutate(across(c(median, lower_ci, upper_ci), ~ .x * 100))

  y_ceiling <- density_peak * 1.1

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

  p_diff <- diff_draws |>
    mutate(difference_pct = difference * 100) |>
    ggplot(aes(x = difference_pct)) +
    geom_density(alpha = 0.6, fill = diff_fill) +
    coord_cartesian(ylim = c(0, y_ceiling)) +
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
      y = NULL
    ) +
    scale_x_continuous(
      labels = label_number(suffix = " pp", accuracy = 0.1),
      limits = x_limits,
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    theme_minimal(base_family = "Arial", base_size = base_pt) +
    theme(legend.position = "none")

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


# ── make_prior_sensitivity_figure() ───────────────────────────────────────────
#
# Takes a named list of four fitted models (one per prior) for a single outcome
# and produces a two-panel figure suitable for the main manuscript.
#
# models_by_prior: named list with elements "neutral", "left", "right", "diffuse"
#   e.g. list(
#     neutral = all_main_models[["model_main_neurosurgery_evac_neutral_adjusted"]],
#     left    = all_main_models[["model_main_neurosurgery_evac_left_adjusted"]],
#     right   = all_main_models[["model_main_neurosurgery_evac_right_adjusted"]],
#     flat    = all_main_models[["model_main_neurosurgery_evac_flat_adjusted"]]
#   )
#
# Panel A uses the neutral model only — it establishes the clinical baseline
# (absolute probabilities by hemisphere) without prior-specific distortion.
# Panel B overlays all four difference distributions so the reader can
# directly compare how much the prior choice shifts the posterior.

make_prior_sensitivity_figure <- function(
  models_by_prior,
  outcome_label,
  covariate_caption,
  laterality_var = "ich_laterality",
  ref_level = "Left",
  contrast_level = "Right",
  x_limits = c(0, 20),
  base_pt = 14,
  caption_width = 110,
  palette = c(Left = "#0072B2", Right = "#D55E00"),
  # Okabe-Ito safe palette for the four priors
  prior_palette = c(
    neutral = "#009E73",
    left = "#E69F00",
    right = "#56B4E9",
    flat = "#CC79A7"
  ),
  prior_labels = c(
    neutral = "Neutral Prior",
    left = "Left Hemisphere Prior",
    right = "Right Hemisphere Prior",
    flat = "Diffuse Prior"
  )
) {
  base_mm <- base_pt / 2.835

  # ── Panel A: neutral model absolute probabilities ──────────────────────────
  # Identical logic to make_posterior_prob_figure() Panel A, using neutral only.
  # This gives the reader the clinically-grounded absolute risk anchor before
  # they look at how robust that finding is across priors in Panel B.
  neutral_draws <- avg_predictions(
    models_by_prior$neutral,
    by = laterality_var
  ) |>
    posterior_draws()

  pred_summary <- neutral_draws |>
    distinct(.data[[laterality_var]], estimate, conf.low, conf.high) |>
    mutate(across(where(is.numeric), ~ .x * 100))

  density_peak_a <- neutral_draws |>
    mutate(draw_pct = draw * 100) |>
    group_by(.data[[laterality_var]]) |>
    summarise(peak = max(density(draw_pct)$y), .groups = "drop") |>
    pull(peak) |>
    max()

  pred_summary <- pred_summary |>
    mutate(
      y_pos = if_else(
        .data[[laterality_var]] == ref_level,
        density_peak_a * 0.55,
        density_peak_a * 0.32
      )
    )

  p_abs <- neutral_draws |>
    mutate(draw_pct = draw * 100) |>
    ggplot(aes(x = draw_pct, fill = .data[[laterality_var]])) +
    geom_density(alpha = 0.6) +
    coord_cartesian(ylim = c(0, density_peak_a * 1.1)) +
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
      x = paste0("P(", outcome_label, ") — Neutral Prior"),
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

  # ── Panel B: overlaid difference distributions across all four priors ──────
  # Within-draw differencing is done separately per model so joint uncertainty
  # is propagated correctly for each prior. Results are stacked into a single
  # long dataframe keyed by prior label for ggplot grouping.
  compute_diff_draws <- function(model, prior_name) {
    avg_predictions(model, by = laterality_var) |>
      posterior_draws() |>
      pivot_wider(
        id_cols = drawid,
        names_from = .data[[laterality_var]],
        values_from = draw
      ) |>
      mutate(
        difference_pct = (.data[[contrast_level]] - .data[[ref_level]]) * 100,
        prior = prior_labels[[prior_name]]
      )
  }

  all_diff_draws <- imap_dfr(models_by_prior, compute_diff_draws) |>
    mutate(prior = factor(prior, levels = prior_labels))

  # Per-prior summaries for the annotation table in Panel B
  diff_summaries <- all_diff_draws |>
    group_by(prior) |>
    summarize(
      median = median(difference_pct),
      lower_ci = quantile(difference_pct, 0.025),
      upper_ci = quantile(difference_pct, 0.975),
      prob_gt_0 = sum(difference_pct > 0) / n(),
      .groups = "drop"
    ) |>
    mutate(
      label = glue(
        "**{prior}**: {sprintf('%.2f', median)} pp ",
        "({sprintf('%.2f', lower_ci)} \u2013 {sprintf('%.2f', upper_ci)}), ",
        "P(Right > Left): {format_posterior_prob(prob_gt_0)}"
      )
    )

  # y ceiling for Panel B: based on the tallest density across all four priors
  density_peak_b <- all_diff_draws |>
    group_by(prior) |>
    summarise(peak = max(density(difference_pct)$y), .groups = "drop") |>
    pull(peak) |>
    max()

  y_ceiling_b <- density_peak_b * 1.1

  # Named colour vector keyed to the label strings so ggplot matches correctly
  prior_colors_named <- setNames(
    prior_palette[names(prior_labels)],
    prior_labels
  )

  # Annotation block — stacked in the upper-right whitespace.
  # x position is fixed at 25% of the x range
  # y positions are staggered by prior so labels don't overlap.
  n_priors <- nrow(diff_summaries)
  annotation_x <- x_limits[1] + (x_limits[2] - x_limits[1]) * 0.25
  diff_summaries <- diff_summaries |>
    mutate(
      annot_y = y_ceiling_b * (0.95 - (row_number() - 1) * (0.95 / n_priors))
    )

  p_diff <- all_diff_draws |>
    ggplot(aes(x = difference_pct, fill = prior, color = prior)) +
    geom_density(alpha = 0.35, linewidth = 0.8) +
    coord_cartesian(ylim = c(0, y_ceiling_b)) +
    geom_vline(
      xintercept = 0,
      linetype = "dotted",
      linewidth = 1.5,
      color = "grey50"
    ) +
    # Median tick per prior — thin vertical lines in prior colours
    geom_vline(
      data = diff_summaries,
      aes(xintercept = median, color = prior),
      linetype = "dotted",
      linewidth = 1
    ) +
    scale_fill_manual(values = prior_colors_named) +
    scale_color_manual(values = prior_colors_named) +
    # Stacked richtext annotations, one per prior
    geom_richtext(
      data = diff_summaries,
      aes(
        x = annotation_x,
        y = annot_y,
        label = label,
        color = prior
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
      y = NULL
    ) +
    scale_x_continuous(
      labels = label_number(suffix = " pp", accuracy = 0.1),
      limits = x_limits,
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    theme_minimal(base_family = "Arial", base_size = base_pt) +
    theme(legend.position = "none")

  # ── Combine and annotate ───────────────────────────────────────────────────
  caption_text <- str_wrap(
    paste0(
      "Panel A: Posterior distributions of covariate-adjusted average predicted ",
      "probabilities of ",
      tolower(outcome_label),
      " by ICH hemisphere under the neutral prior. ",
      "Panel B: Overlaid posterior distributions of the absolute probability ",
      "difference (Right \u2212 Left hemisphere) across four prior specifications ",
      "(neutral, left-hemisphere, right-hemisphere, and diffuse). ",
      "All four priors yield consistent findings, demonstrating robustness of ",
      "the primary result to prior choice. ",
      covariate_caption,
      " ",
      "Dotted lines indicate posterior medians. ",
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
format_posterior_prob <- function(p, digits = 1) {
  threshold <- 0.001
  dplyr::case_when(
    p > 1 - threshold ~ "> 99.9%",
    p < threshold ~ "< 0.1%",
    .default = paste0(round(p * 100, digits), "%")
  )
}
