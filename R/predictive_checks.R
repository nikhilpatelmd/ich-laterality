

f_general_pp_check <- function(model, ndraws = 100) {
  # 1. Extract the model family
  mod_family <- stats::family(model)$family

  # 2. Branch visualizations based on the model type
  if (mod_family %in% c("bernoulli", "binomial", "cumulative")) {
    # --- Binary and Ordinal Outcomes ---
    # Best checked with bar plots
    p1 <- brms::pp_check(model, type = "bars", ndraws = ndraws) +
      ggplot2::labs(title = paste(mod_family, "- Overall Fit"))

    p2 <- brms::pp_check(
      model,
      type = "bars_grouped",
      group = "ich_laterality",
      ndraws = ndraws
    ) +
      ggplot2::labs(title = "Fit Grouped by Laterality")

    return(p1 / p2)
  } else if (
    mod_family %in% c("negbinomial", "zero_inflated_negbinomial", "poisson")
  ) {
    # --- Count Outcomes (Ventilation) ---
    # Best checked with a density overlay and a specific check for the zeroes
    p1 <- brms::pp_check(model, type = "dens_overlay", ndraws = ndraws) +
      ggplot2::labs(title = paste(mod_family, "- Density Overlay"))

    # Custom statistic: Proportion of exactly 0 days
    p2 <- brms::pp_check(
      model,
      type = "stat",
      stat = function(y) mean(y == 0),
      ndraws = ndraws
    ) +
      ggplot2::labs(title = "Proportion of Zeroes")

    return(p1 / p2)
  } else if (mod_family == "zero_one_inflated_beta") {
    # --- ZOIB Outcomes (Euro VAS) ---
    # Requires checking the continuous distribution AND the floor/ceiling effects
    p1 <- brms::pp_check(model, type = "dens_overlay", ndraws = ndraws) +
      ggplot2::labs(title = "ZOIB - Density Overlay")

    # Check the floor (Prop of 0s)
    p2 <- brms::pp_check(
      model,
      type = "stat",
      stat = function(y) mean(y == 0),
      ndraws = ndraws
    ) +
      ggplot2::labs(title = "Floor (Score = 0)")

    # Check the ceiling (Prop of 1s)
    p3 <- brms::pp_check(
      model,
      type = "stat",
      stat = function(y) mean(y == 1),
      ndraws = ndraws
    ) +
      ggplot2::labs(title = "Ceiling (Score = 1)")

    return(p1 / (p2 | p3))
  } else {
    # --- Fallback for continuous/other models ---
    p1 <- brms::pp_check(model, type = "dens_overlay", ndraws = ndraws) +
      ggplot2::labs(title = paste(mod_family, "- Density Overlay"))

    return(p1)
  }
}
