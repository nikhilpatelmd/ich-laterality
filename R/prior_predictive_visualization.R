# R/prior_predictive_visualization.R
#
# Prior predictive check forest plots.
#
# Two public functions are exported:
#   make_ppc_forest_aggressive()  — binary + count outcomes (Table 2 family)
#   make_ppc_forest_functional()  — ordinal + VAS outcomes  (Table 4 family)
#
# Each figure shows outcomes on the y-axis with four dodged point-range rows
# per outcome (one per prior specification), making cross-prior comparison
# scannable at a glance. The computational path for extracting medians and
# 95% CIs mirrors table_2.R and table_3.R exactly so the numbers are
# consistent between the tables and their visual summaries.

# ── Shared constants ───────────────────────────────────────────────────────────

# Okabe-Ito palette — same assignment used throughout the manuscript
.forest_palette <- c(
  "Neutral" = "#000000",
  "Left-skeptical" = "#0072B2",
  "Right-skeptical" = "#D55E00",
  "Flat" = "#009E73"
)

# Factor levels used consistently across both figures so legend order is stable
.prior_levels <- c("Neutral", "Left-skeptical", "Right-skeptical", "Flat")
.prior_raw <- c("neutral", "left", "right", "flat")

.relabel_prior_forest <- function(x) {
  factor(x, levels = .prior_raw, labels = .prior_levels)
}

# Shared ggplot theme — keeps both figures visually identical
.forest_theme <- function(base_mm) {
  theme_bw(base_size = base_mm) +
    theme(
      legend.position = "top",
      legend.title = element_text(face = "bold"),
      panel.grid.major.y = element_blank(), # horizontal grid clutters a forest plot
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      axis.text.y = element_text(size = base_mm * 0.9),
      plot.title = element_text(face = "bold")
    )
}


# ── Summary-extraction helpers ─────────────────────────────────────────────────

# Extract median + 95% CI for one binary/bernoulli prior model.
# Mirrors get_marginal_stats() in table_2.R: lnoravg → exp() gives the OR.
.summarize_binary_prior <- function(model) {
  marginaleffects::avg_comparisons(
    model,
    variables = "ich_laterality",
    comparison = "lnoravg"
  ) |>
    marginaleffects::posterior_draws() |>
    mutate(draw = exp(draw)) |> # log-OR → OR
    summarize(
      estimate = median(draw),
      lo = quantile(draw, 0.025),
      hi = quantile(draw, 0.975)
    )
}

# Extract median + 95% CI for one count prior model (zero-inflated NegBin).
# Mirrors the count branch in get_marginal_stats() in table_2.R.
.summarize_count_prior <- function(model) {
  marginaleffects::avg_comparisons(
    model,
    variables = "ich_laterality",
    comparison = function(hi, lo) hi / lo # IRR
  ) |>
    marginaleffects::posterior_draws() |>
    summarize(
      estimate = median(draw),
      lo = quantile(draw, 0.025),
      hi = quantile(draw, 0.975)
    )
}

# Extract median + 95% CI for one ordinal cumulative prior model.
# Mirrors process_ordinal() in table_3.R: within-draw averaging over drawid
# collapses threshold-specific contrasts to a single population-averaged OR.
.summarize_ordinal_prior <- function(model) {
  marginaleffects::avg_comparisons(
    model,
    variables = "ich_laterality",
    comparison = "lnoravg"
  ) |>
    marginaleffects::posterior_draws() |>
    group_by(drawid) |>
    summarize(log_or = mean(draw, na.rm = TRUE), .groups = "drop") |>
    filter(!is.na(log_or)) |>
    mutate(draw = exp(log_or)) |>
    summarize(
      estimate = median(draw),
      lo = quantile(draw, 0.025),
      hi = quantile(draw, 0.975)
    )
}

# Extract median + 95% CI for the VAS ZOIB prior model as a mean difference.
# Mirrors process_vas() in table_3.R: G-computation via posterior_epred.
# `data` is ich_aggressive — covariates only, no outcome data consumed.
.summarize_vas_prior <- function(model, data) {
  used_vars <- setdiff(all.vars(stats::formula(model)$formula), "euro_vas_90")
  clean_data <- as.data.frame(data) |>
    tidyr::drop_na(dplyr::any_of(used_vars))

  data_left <- clean_data
  data_right <- clean_data
  data_left$ich_laterality <- factor("Left", levels = c("Left", "Right"))
  data_right$ich_laterality <- factor("Right", levels = c("Left", "Right"))

  estim <- (rowMeans(brms::posterior_epred(model, newdata = data_right)) -
    rowMeans(brms::posterior_epred(model, newdata = data_left))) *
    100

  tibble::tibble(
    estimate = median(estim),
    lo = quantile(estim, 0.025),
    hi = quantile(estim, 0.975)
  )
}

# General collector: loops over the four prior scenarios for one outcome,
# applies `summary_fn`, and returns a tidy tibble ready to bind with others.
# `summary_fn` receives the model object (and optionally `data` via ...).
.collect_prior_summaries <- function(
  all_prior_models,
  outcome_col,
  outcome_label,
  summary_fn,
  ...
) {
  purrr::map_dfr(
    .prior_raw,
    function(p) {
      key <- paste0("model_prior_", outcome_col, "_", p, "_adjusted")
      model <- all_prior_models[[key]]
      summary_fn(model, ...) |>
        mutate(outcome = outcome_label, prior = p)
    }
  )
}


# ── Public figure functions ────────────────────────────────────────────────────

#' Prior predictive check forest plot — aggressive care outcomes (Table 2 family)
#'
#' Binary outcomes and the count outcome (days mechanical ventilation) are shown
#' in separate facet panels so the axis label can distinguish OR from IRR,
#' though both live on the same log scale. Reference lines follow the same
#' ROPE / SESOI thresholds used in the manuscript tables.
#'
#' @param all_prior_models Named list from tar_combine(all_prior_models, ...).
#' @return A ggplot object.
make_ppc_forest_aggressive <- function(all_prior_models) {
  base_pt <- 14
  base_mm <- base_pt / 2.835

  # ── Collect summaries ──────────────────────────────────────────────────────
  # Binary outcomes — OR scale
  binary_defs <- list(
    list(col = "neurosurgery_evac", label = "Neurosurgical Intervention"),
    list(col = "evd", label = "EVD Placement"),
    list(col = "tracheostomy", label = "Tracheostomy"),
    list(col = "comfort_care_binary", label = "Comfort Care / WLST"),
    list(col = "early_wlst", label = "Early WLST"),
    list(col = "dnr_binary", label = "DNR Order")
  )

  df_binary <- purrr::map_dfr(binary_defs, function(o) {
    .collect_prior_summaries(
      all_prior_models,
      o$col,
      o$label,
      .summarize_binary_prior
    )
  }) |>
    mutate(panel = "Binary outcomes (Odds Ratio)")

  df_count <- .collect_prior_summaries(
    all_prior_models,
    "days_mechanical_ventilation",
    "Days of Mechanical Ventilation",
    .summarize_count_prior
  ) |>
    mutate(panel = "Count outcome (Incidence Rate Ratio)")

  df <- bind_rows(df_binary, df_count) |>
    mutate(
      # Factor outcome within each panel so facets are ordered independently
      outcome = factor(
        outcome,
        levels = rev(c(
          purrr::map_chr(binary_defs, "label"),
          "Days of Mechanical Ventilation"
        ))
      ),
      prior = .relabel_prior_forest(prior),
      # Facet order: binary panel first, count panel second
      panel = factor(
        panel,
        levels = c(
          "Binary outcomes (Odds Ratio)",
          "Count outcome (Incidence Rate Ratio)"
        )
      )
    )

  # ── Reference line data ────────────────────────────────────────────────────
  # Drawn as geom_vline so they appear behind the point ranges. Three lines:
  #   solid  — null (OR/IRR = 1.0)
  #   dashed — SESOI threshold (1.2)
  #   dotted — ROPE boundaries (0.95, 1.05)
  ref_lines <- list(
    geom_vline(
      xintercept = 1,
      linetype = "solid",
      colour = "grey30",
      linewidth = 0.4
    ),
    geom_vline(
      xintercept = 1.2,
      linetype = "dashed",
      colour = "grey50",
      linewidth = 0.35
    ),
    geom_vline(
      xintercept = 0.95,
      linetype = "dotted",
      colour = "grey60",
      linewidth = 0.3
    ),
    geom_vline(
      xintercept = 1.05,
      linetype = "dotted",
      colour = "grey60",
      linewidth = 0.3
    )
  )

  # ── Build plot ─────────────────────────────────────────────────────────────
  # position_dodge shifts the four prior rows apart vertically so they don't
  # overlap; width = 0.5 gives comfortable spacing within each outcome strip.
  ggplot(
    df,
    aes(x = estimate, y = outcome, colour = prior, xmin = lo, xmax = hi)
  ) +
    ref_lines +
    geom_pointrange(
      position = position_dodge(width = 0.6),
      size = 0.35,
      linewidth = 0.55
    ) +
    scale_colour_manual(
      values = .forest_palette,
      name = "Prior specification"
    ) +
    scale_x_log10(
      labels = scales::label_number(accuracy = 0.1),
      breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20)
    ) +
    # Free y scales lets each facet show only its own outcomes without empty rows
    facet_wrap(~panel, ncol = 1, scales = "free_y") +
    labs(
      title = "Prior predictive checks \u2014 aggressive care outcomes",
      x = "Prior effect estimate (log scale)",
      y = NULL,
      caption = paste0(
        "Solid line: null (OR/IRR = 1.0).  ",
        "Dashed line: SESOI threshold (1.2).  ",
        "Dotted lines: ROPE boundaries (0.95\u20131.05).\n",
        "Points show prior median; whiskers show 95% prior credible interval. ",
        "All estimates derived from prior samples only."
      )
    ) +
    .forest_theme(base_mm) +
    theme(
      plot.caption = element_text(size = base_mm * 0.82, colour = "grey40"),
      # The two facets have very different numbers of outcomes, so nudge the
      # binary panel to occupy more vertical space via strip padding rather
      # than distorting the scales
      panel.spacing = unit(1.2, "lines")
    )
}


#' Prior predictive check forest plot — functional outcomes (Table 4 family)
#'
#' Ordinal outcomes (OR scale) and Euro VAS (mean difference in points) require
#' different x-axes, so they are combined via patchwork rather than facets.
#' This mirrors the two-estimand structure of table_3.R.
#'
#' @param all_prior_models Named list from tar_combine(all_prior_models, ...).
#' @param data ich_aggressive data frame; needed for the VAS G-computation
#'   covariate grid (outcome values are not used).
#' @return A patchwork object.
make_ppc_forest_functional <- function(all_prior_models, data) {
  base_pt <- 14
  base_mm <- base_pt / 2.835

  # ── Collect summaries ──────────────────────────────────────────────────────
  ordinal_defs <- list(
    list(col = "mrs_90", label = "Modified Rankin Scale"),
    list(col = "euro_mobility_90", label = "EuroQOL \u2014 Mobility"),
    list(col = "euro_selfcare_90", label = "EuroQOL \u2014 Self-Care"),
    list(col = "euro_usual_90", label = "EuroQOL \u2014 Usual Activities"),
    list(col = "euro_pain_90", label = "EuroQOL \u2014 Pain / Discomfort"),
    list(col = "euro_anxiety_90", label = "EuroQOL \u2014 Anxiety / Depression")
  )

  df_ordinal <- purrr::map_dfr(ordinal_defs, function(o) {
    .collect_prior_summaries(
      all_prior_models,
      o$col,
      o$label,
      .summarize_ordinal_prior
    )
  }) |>
    mutate(
      outcome = factor(
        outcome,
        levels = rev(purrr::map_chr(ordinal_defs, "label"))
      ),
      prior = .relabel_prior_forest(prior)
    )

  df_vas <- .collect_prior_summaries(
    all_prior_models,
    "euro_vas_90",
    "Euro VAS",
    .summarize_vas_prior,
    data = data # passed through ... to .summarize_vas_prior
  ) |>
    mutate(
      outcome = factor("Euro VAS"),
      prior = .relabel_prior_forest(prior)
    )

  # ── Panel A: Ordinal outcomes (OR scale, log x-axis) ──────────────────────
  panel_a <- ggplot(
    df_ordinal,
    aes(x = estimate, y = outcome, colour = prior, xmin = lo, xmax = hi)
  ) +
    geom_vline(
      xintercept = 1,
      linetype = "solid",
      colour = "grey30",
      linewidth = 0.4
    ) +
    geom_vline(
      xintercept = 1.2,
      linetype = "dashed",
      colour = "grey50",
      linewidth = 0.35
    ) +
    geom_vline(
      xintercept = 0.95,
      linetype = "dotted",
      colour = "grey60",
      linewidth = 0.3
    ) +
    geom_vline(
      xintercept = 1.05,
      linetype = "dotted",
      colour = "grey60",
      linewidth = 0.3
    ) +
    geom_pointrange(
      position = position_dodge(width = 0.6),
      size = 0.35,
      linewidth = 0.55
    ) +
    scale_colour_manual(
      values = .forest_palette,
      name = "Prior specification"
    ) +
    scale_x_log10(
      labels = scales::label_number(accuracy = 0.1),
      breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10)
    ) +
    labs(
      title = "A   Ordinal outcomes (population-averaged Odds Ratio)",
      x = "Prior OR — Right vs Left hemisphere (log scale)",
      y = NULL
    ) +
    .forest_theme(base_mm)

  # ── Panel B: Euro VAS (mean difference, linear x-axis) ────────────────────
  # Different reference lines because the estimand is a mean difference:
  #   solid  — no difference (0 points)
  #   dashed — SESOI (±5 points)
  #   dotted — ROPE (±2 points)
  panel_b <- ggplot(
    df_vas,
    aes(x = estimate, y = outcome, colour = prior, xmin = lo, xmax = hi)
  ) +
    geom_vline(
      xintercept = 0,
      linetype = "solid",
      colour = "grey30",
      linewidth = 0.4
    ) +
    geom_vline(
      xintercept = -5,
      linetype = "dashed",
      colour = "grey50",
      linewidth = 0.35
    ) +
    geom_vline(
      xintercept = 5,
      linetype = "dashed",
      colour = "grey50",
      linewidth = 0.35
    ) +
    geom_vline(
      xintercept = -2,
      linetype = "dotted",
      colour = "grey60",
      linewidth = 0.3
    ) +
    geom_vline(
      xintercept = 2,
      linetype = "dotted",
      colour = "grey60",
      linewidth = 0.3
    ) +
    geom_pointrange(
      position = position_dodge(width = 0.6),
      size = 0.35,
      linewidth = 0.55
    ) +
    scale_colour_manual(
      values = .forest_palette,
      name = "Prior specification"
    ) +
    labs(
      title = "B   Euro VAS (mean difference, Right \u2212 Left, points)",
      x = "Prior mean difference in VAS score (points)",
      y = NULL
    ) +
    .forest_theme(base_mm) +
    # Legend is shown in Panel A; suppress duplicate
    theme(legend.position = "none")

  # ── Combine via patchwork ──────────────────────────────────────────────────
  # Heights weighted by number of outcomes: 6 ordinal rows vs 1 VAS row.
  # A minimum Panel B height of ~1.5 keeps the single row readable.
  patchwork::wrap_plots(panel_a, panel_b, ncol = 1, heights = c(6, 1.5)) +
    patchwork::plot_annotation(
      caption = paste0(
        "Panel A \u2014 solid: null (OR 1.0); dashed: SESOI (OR 1.2); dotted: ROPE (OR 0.95\u20131.05).\n",
        "Panel B \u2014 solid: no difference (0 pts); dashed: SESOI (\u00b15 pts); dotted: ROPE (\u00b12 pts).\n",
        "Points show prior median; whiskers show 95% prior credible interval. ",
        "All estimates derived from prior samples only."
      ),
      theme = theme(
        plot.caption = element_text(size = base_mm * 0.82, colour = "grey40")
      )
    )
}
