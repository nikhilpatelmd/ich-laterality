library(marginaleffects)
library(ggplot2)
library(ggdist)
library(patchwork)
library(dplyr)
library(tidyr)
library(scales)

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 2: Absolute Predicted Probabilities — Neurosurgical Intervention
#
# Strategy overview:
#   Left panel  → Full posterior of P(surgery | Left) and P(surgery | Right),
#                 each averaged over the observed covariate distribution
#                 (i.e., the epidemiological "marginal" probability).
#   Right panel → Full posterior of the absolute difference: P(Right) − P(Left),
#                 with a reference line at 0 and annotation of P(diff > 0).
#
# Why avg_predictions() rather than predictions()?
#   predictions() returns one predicted probability per patient per draw — a
#   dataset the size of N_patients × N_draws. avg_predictions(by = "group")
#   averages within each group at each draw, giving us a single number per
#   group per draw: the Average Marginal Prediction (AMP). This is precisely
#   the population-averaged absolute probability the reviewer is asking for,
#   and it is consistent with the AME approach already used in Table 2.
# ══════════════════════════════════════════════════════════════════════════════

make_figure_2 <- function(
  model,
  outcome_label = "Neurosurgical Intervention",
  laterality_var = "ich_laterality",
  ref_level = "Left", # level that is the "comparator"
  contrast_level = "Right", # level whose probability is typically higher
  palette = c(Left = "#2166ac", Right = "#d6604d"),
  diff_fill = "#6a3d9a" # neutral colour for the difference panel
) {
  # ── Step 1: Average predicted probabilities per laterality level ─────────
  # avg_predictions(by = laterality_var) instructs marginaleffects to:
  #   (a) generate predictions for every patient in the dataset,
  #   (b) average those predictions within each level of ich_laterality.
  # When called on a brms model, posterior_draws() then unpacks the MCMC
  # samples so we get one row per (draw × laterality level), with the
  # predicted probability in the column `draw`.
  pred_draws <- avg_predictions(
    model,
    by = laterality_var
  ) |>
    posterior_draws()

  # ── Step 2: Draw-level difference (contrast − reference) ─────────────────
  # We pivot wide so that Left and Right are columns on the same row,
  # then subtract. Crucially, this is done *within* each drawid, so
  # the resulting posterior of the difference correctly propagates the
  # joint uncertainty from both group estimates rather than working from
  # just their marginal summaries.
  diff_draws <- pred_draws |>
    select(drawid, all_of(laterality_var), draw) |>
    pivot_wider(names_from = all_of(laterality_var), values_from = draw) |>
    mutate(diff = .data[[contrast_level]] - .data[[ref_level]])

  # ── Step 3: Annotation summaries ─────────────────────────────────────────
  # We compute median and 95% credible intervals for each group and for
  # the difference. These will be placed as text labels on the figure.
  pred_summary <- pred_draws |>
    group_by(across(all_of(laterality_var))) |>
    summarize(
      estimate = median(draw),
      lower = quantile(draw, 0.025),
      upper = quantile(draw, 0.975),
      .groups = "drop"
    ) |>
    mutate(
      label = sprintf(
        "%.1f%%\n(%.1f\u2013%.1f%%)",
        estimate * 100,
        lower * 100,
        upper * 100
      )
    )

  diff_summary <- diff_draws |>
    summarize(
      estimate = median(diff),
      lower = quantile(diff, 0.025),
      upper = quantile(diff, 0.975),
      # Posterior probability that Right > Left on the absolute scale
      prob_pos = mean(diff > 0)
    ) |>
    mutate(
      label = sprintf(
        "\u0394 = %+.1f pp\n(%.1f to %+.1f)",
        estimate * 100,
        lower * 100,
        upper * 100
      )
    )

  # ── Step 4: Left panel — absolute posterior distributions ────────────────
  # stat_halfeye() from ggdist draws two layers simultaneously:
  #   (1) A "slab" — a smoothed density of the posterior samples, visualising
  #       the full shape of uncertainty. This sits above the axis.
  #   (2) A "point interval" — a dot at the median with a horizontal line
  #       spanning the 95% CrI. This sits along the axis baseline.
  # Together these communicate the central estimate, interval, and distributional
  # shape, making it immediately clear whether the posterior is symmetric,
  # skewed, or multimodal.
  #
  # We map y to ich_laterality so the two groups stack vertically, which
  # is more space-efficient than facets and makes the overlap easy to judge.
  p_abs <- pred_draws |>
    mutate(draw_pct = draw * 100) |>
    ggplot(aes(
      x = draw_pct,
      y = .data[[laterality_var]],
      fill = .data[[laterality_var]],
      color = .data[[laterality_var]]
    )) +
    ggdist::stat_halfeye(
      .width = 0.95, # show the 95% CrI in the point-interval layer
      slab_alpha = 0.65, # semi-transparent slab so overlap is readable
      normalize = "panels", # scale each slab to the same max height
      point_interval = "median_qi"
    ) +
    # Annotation: place the point estimate + CrI label to the right of the peak.
    # We nudge upward by adjusting vjust; exact placement may need tuning
    # depending on the spread of the posterior in your data.
    geom_text(
      data = pred_summary |>
        mutate(estimate_pct = estimate * 100), # just this; no mutate/rename dance
      aes(
        x = estimate_pct,
        y = .data[[laterality_var]],
        label = label
      ),
      inherit.aes = FALSE,
      nudge_y = 0.35,
      size = 3.0,
      fontface = "bold",
      lineheight = 0.9
    ) +
    scale_fill_manual(values = palette, guide = "none") +
    scale_color_manual(values = palette, guide = "none") +
    # Format x-axis as percentage; expand rightward to give annotation room
    scale_x_continuous(
      labels = label_number(suffix = "%", accuracy = 1),
      limits = c(0, NA),
      expand = expansion(mult = c(0.02, 0.20))
    ) +
    # y-axis: we flip the order so Left is on top, matching convention
    scale_y_discrete(limits = rev(c(ref_level, contrast_level))) +
    labs(
      x = "Predicted Probability of Neurosurgical Intervention",
      y = NULL,
      title = "A",
    ) +
    theme_minimal(base_family = "Arial", base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 9, color = "grey40"),
      axis.text.y = element_text(
        size = 10,
        face = "bold",
        colour = palette[rev(c(ref_level, contrast_level))]
      )
    )

  # ── Step 5: Right panel — posterior of the absolute difference ────────────
  # This panel shows a single distribution: P(Right) − P(Left).
  # Positive values mean Right-hemisphere patients had a higher probability.
  # The dashed reference line at 0 acts as a visual null hypothesis.
  #
  # We set y = "" (a length-1 factor) so ggdist renders a single halfeye
  # horizontally. The panel is deliberately sparse to contrast with the
  # two-group left panel.
  p_diff <- diff_draws |>
    mutate(diff_pct = diff * 100) |>
    ggplot(aes(x = diff_pct, y = "")) +
    # Null reference: no difference between hemispheres
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "grey55",
      linewidth = 0.6
    ) +
    ggdist::stat_halfeye(
      .width = 0.95,
      slab_alpha = 0.65,
      fill = diff_fill,
      color = diff_fill,
      point_interval = "median_qi"
    ) +
    # Primary annotation: Δ with CrI
    geom_text(
      data = diff_summary |> mutate(diff_pct = estimate * 100),
      aes(x = diff_pct, y = "", label = label),
      inherit.aes = FALSE,
      nudge_y = 0.50,
      size = 3.0,
      fontface = "bold",
      lineheight = 0.9
    ) +
    # Secondary annotation: posterior probability that Right > Left
    # Placed below the interval for visual separation
    geom_text(
      data = diff_summary |> mutate(diff_pct = estimate * 100),
      aes(
        x = diff_pct,
        y = "",
        label = sprintf("P(Right > Left) = %.2f", prob_pos)
      ),
      inherit.aes = FALSE,
      nudge_y = -0.28,
      size = 2.8,
      color = "grey30",
      fontface = "italic"
    ) +
    scale_x_continuous(
      labels = label_number(suffix = " %", accuracy = 0.1),
      expand = expansion(mult = c(0.15, 0.15))
    ) +
    labs(
      x = "Absolute Difference",
      y = NULL,
      title = "B",
      subtitle = paste0(contrast_level, " \u2212 ", ref_level, " hemisphere")
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 9, color = "grey40")
    )

  # ── Step 6: Combine panels with patchwork ─────────────────────────────────
  # We give the left panel slightly more horizontal space (widths = c(1.3, 1))
  # because it holds two distributions and the y-axis labels. The overall title
  # and caption sit outside both panels via plot_annotation().
  combined <- p_abs /
    p_diff +
    plot_layout(heights = c(1.8, 1)) +
    plot_annotation(
      caption = paste0(
        "Posterior distributions of covariate-adjusted average predicted probabilities.\n",
        "Estimates reflect the population-averaged probability across the observed covariate\n",
        "distribution. Dots indicate posterior medians; horizontal bars span 95% credible intervals.\n",
        "pp = percentage points."
      ),
      theme = theme(
        plot.title = element_text(face = "bold", size = 13),
        plot.caption = element_text(size = 8, color = "grey40", hjust = 0)
      )
    )

  return(combined)
}

# ── Usage ────────────────────────────────────────────────────────────────────
# Assuming your targets pipeline has loaded the model into `model_neurosurgery`:
#
#   fig2 <- make_figure_2(
#     model        = model_neurosurgery,
#     outcome_label = "Neurosurgical Intervention"
#   )
#
#   ggsave(
#     "figure_2_abs_probs.pdf",
#     plot   = fig2,
#     width  = 10,
#     height = 4.5,
#     device = cairo_pdf   # for clean font rendering in PDFs
#   )
#
# ── Integrating into targets ──────────────────────────────────────────────────
# In your _targets.R, add a target like:
#
#   tar_target(
#     figure_2,
#     make_figure_2(
#       model        = model_main_neurosurgery_evac_neutral_adjusted,
#       outcome_label = "Neurosurgical Intervention"
#     ),
#     deployment = "main"   # consistent with your brms/CmdStan pattern
#   )
#
# ── A note on multiple imputation ────────────────────────────────────────────
# If your brms model was fit with brm_multiple() across MICE-imputed datasets,
# avg_predictions() should still work on the combined model object. However,
# marginaleffects will use the stacked imputed datasets stored in model$data.
# You may want to pass newdata = original_imputed_data explicitly if you notice
# the draws count is unexpectedly large (= N_imputations × N_patients per draw).
