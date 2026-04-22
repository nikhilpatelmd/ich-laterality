# R/vas_figure.R
#
# Two-panel EuroQOL VAS figure for the ICH laterality study.
#
# The VAS is modelled as a zero-one inflated beta (ZOIB) outcome scaled to
# [0, 1]; all posterior quantities are rescaled ×100 here for display in the
# natural 0–100 point metric that clinicians read.
#
# Because ZOIB models have no `group` dimension in avg_predictions(), the
# correct estimand is the marginal expected value E[VAS | laterality],
# obtained via G-computation with posterior_epred(). This mirrors the approach
# in process_vas() (table_3.R) and .summarize_vas_prior()
# (prior_predictive_visualization.R).
#
# Entry point: make_vas_figure(model, data)
#   model — a brmsfit or brmsfit_multiple fitted by fit_vas_zoib()
#   data  — ich_aggressive (covariates only; outcome not used)
#
# Panel A: Overlapping posterior densities of E[VAS | Left] and E[VAS | Right],
#          with dashed median segments clipped at the KDE peak — same aesthetic
#          as make_euro_uncertainty() so both figures read consistently when
#          placed side-by-side in the supplement.
#
# Panel B: Posterior density of the mean difference E[VAS | Right] − E[VAS |
#          Left] in VAS points, with a reference line at zero and a posterior
#          probability annotation.


# ── Shared constants ───────────────────────────────────────────────────────────

# Okabe-Ito palette — consistent with laterality colouring used throughout
.vas_palette <- c("Left" = "#0072B2", "Right" = "#D55E00")

BASE_PT_VAS <- 14


# ── G-computation helper ───────────────────────────────────────────────────────

# .vas_draws() returns a tibble with one row per posterior draw per group,
# columns: drawid (int), ich_laterality (chr), vas_pts (dbl, 0–100 scale).
#
# Design mirrors process_vas() exactly: drop NAs only for variables used by
# this specific model, then duplicate the covariate grid under each laterality
# value and call posterior_epred(). Row-averaging collapses the patient
# dimension within each draw, giving the marginal expected value.
.vas_draws <- function(model, data) {
  # Identify model formula variables; exclude the scaled outcome so we don't
  # accidentally drop rows that have a missing raw VAS but valid covariates.
  used_vars <- setdiff(
    all.vars(stats::formula(model)$formula),
    c("euro_vas_90", "euro_vas_90_scaled")
  )
  clean_data <- as.data.frame(data) |>
    tidyr::drop_na(dplyr::any_of(used_vars)) |>
    dplyr::mutate(euro_vas_90_scaled = dplyr::coalesce(euro_vas_90 / 100, 0.5))

  data_left  <- dplyr::mutate(
    clean_data,
    ich_laterality = factor("Left", levels = c("Left", "Right"))
  )
  data_right <- dplyr::mutate(
    clean_data,
    ich_laterality = factor("Right", levels = c("Left", "Right"))
  )

  # posterior_epred() returns draws × observations; rowMeans gives one value
  # per draw representing the population-averaged expected VAS.
  epred_left  <- brms::posterior_epred(model, newdata = data_left,  allow_new_levels = TRUE)
  epred_right <- brms::posterior_epred(model, newdata = data_right, allow_new_levels = TRUE)

  n_draws <- nrow(epred_left)

  dplyr::bind_rows(
    tibble::tibble(
      drawid        = seq_len(n_draws),
      ich_laterality = "Left",
      vas_pts       = rowMeans(epred_left)  * 100
    ),
    tibble::tibble(
      drawid        = seq_len(n_draws),
      ich_laterality = "Right",
      vas_pts       = rowMeans(epred_right) * 100
    )
  )
}


# ── Panel A: Overlapping posterior predictive densities ───────────────────────

make_vas_density <- function(model, data) {
  draws <- .vas_draws(model, data)

  # Compute per-group medians and clip dashed segment at the KDE peak —
  # identical aesthetic to make_euro_uncertainty().
  medians <- draws |>
    dplyr::group_by(ich_laterality) |>
    dplyr::summarise(
      median_vas = median(vas_pts),
      density_at_median = {
        d <- density(vas_pts)
        stats::approx(d$x, d$y, xout = median(vas_pts))$y
      },
      .groups = "drop"
    )

  ggplot2::ggplot(
    draws,
    ggplot2::aes(x = vas_pts, fill = ich_laterality, colour = ich_laterality)
  ) +
    ggplot2::geom_segment(
      data = medians,
      ggplot2::aes(
        x    = median_vas,
        xend = median_vas,
        y    = 0,
        yend = density_at_median,
        colour = ich_laterality
      ),
      linewidth  = 0.7,
      linetype   = "dashed",
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    ggplot2::geom_density(alpha = 0.35, linewidth = 0.6) +
    ggplot2::scale_x_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, by = 20),
      labels = scales::label_number(accuracy = 1)
    ) +
    ggplot2::scale_fill_manual(values   = .vas_palette, name = "ICH laterality") +
    ggplot2::scale_colour_manual(values = .vas_palette, name = "ICH laterality") +
    ggplot2::guides(
      fill   = ggplot2::guide_legend(override.aes = list(linetype = 0, linewidth = 0)),
      colour = ggplot2::guide_legend(override.aes = list(linetype = 0, linewidth = 0))
    ) +
    ggplot2::labs(
      x = "Expected EuroQOL VAS score at 90 days (0\u2013100 points)",
      y = "Posterior density"
    ) +
    ggplot2::theme_minimal(base_size = BASE_PT_VAS, base_family = "Arial") +
    ggplot2::theme(
      legend.position  = "right",
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.x     = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      axis.title.y     = ggplot2::element_text(margin = ggplot2::margin(r = 10))
    )
}


# ── Panel B: Mean difference (Right − Left) ───────────────────────────────────

make_vas_difference <- function(model, data) {
  draws <- .vas_draws(model, data)

  diff_draws <- draws |>
    tidyr::pivot_wider(
      id_cols     = drawid,
      names_from  = ich_laterality,
      values_from = vas_pts
    ) |>
    dplyr::mutate(difference = Right - Left)

  prob_right_higher <- mean(diff_draws$difference > 0)
  prob_label <- glue::glue(
    "P(Right > Left) = {format_posterior_prob(prob_right_higher)}"
  )

  median_diff <- median(diff_draws$difference)
  density_at_median <- {
    d <- density(diff_draws$difference)
    stats::approx(d$x, d$y, xout = median_diff)$y
  }

  ggplot2::ggplot(diff_draws, ggplot2::aes(x = difference)) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype   = "solid",
      colour     = "grey30",
      linewidth  = 0.4
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x    = median_diff,
        xend = median_diff,
        y    = 0,
        yend = density_at_median
      ),
      colour     = "grey30",
      linewidth  = 0.7,
      linetype   = "dashed"
    ) +
    ggplot2::geom_density(
      fill      = "#7B2D8B",
      colour    = "#7B2D8B",
      alpha     = 0.35,
      linewidth = 0.6
    ) +
    ggtext::geom_richtext(
      data = tibble::tibble(x = Inf, y = Inf, label = prob_label),
      ggplot2::aes(x = x, y = y, label = label),
      hjust        = 1.05,
      vjust        = 1.3,
      fill         = "white",
      label.colour = NA,
      size         = (BASE_PT_VAS * 0.85 / 1.15) / 2.835,
      family       = "Arial"
    ) +
    ggplot2::labs(
      x = "Mean difference in VAS score at 90 days: Right \u2212 Left (points)",
      y = "Posterior density"
    ) +
    ggplot2::theme_minimal(base_size = BASE_PT_VAS, base_family = "Arial") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.x     = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      axis.title.y     = ggplot2::element_text(margin = ggplot2::margin(r = 10))
    )
}


# ── Combined two-panel figure ─────────────────────────────────────────────────

#' @param model A brmsfit or brmsfit_multiple fitted by fit_vas_zoib().
#' @param data  The observed dataset (ich_aggressive). Covariates only — the
#'   outcome is not used. Must be passed explicitly so that targets registers
#'   ich_aggressive as an upstream dependency of each VAS figure target.
#' @return A patchwork object (Panel A over Panel B).
make_vas_figure <- function(model, data) {
  panel_a <- make_vas_density(model, data)
  panel_b <- make_vas_difference(model, data)

  (panel_a / panel_b) +
    patchwork::plot_annotation(tag_levels = "A") +
    patchwork::plot_layout(heights = c(1, 1), guides = "keep")
}