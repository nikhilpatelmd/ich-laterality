# R/table_subgroups.R
#
# Supplemental tables:
#   1. Subgroup interaction analysis (laterality x ICH location, laterality x study)
#   2. ATACH-2 site-level clustering sensitivity analysis
#
# ---------------------------------------------------------------------------
# FORMATTING NOTE (do not reintroduce gt::fmt() here)
#
# Probability columns are formatted to character *eagerly*, while the targets
# pipeline builds the table -- the same pattern used in R/table_2.R and
# R/table_3.R.
#
# gt::fmt() must NOT be used with fmt_prob(). fmt() does not format anything at
# build time; it stores the formatting closure inside the gt_tbl object and
# evaluates it lazily at print time. When the object is serialized by targets
# and read back with tar_read() inside the Quarto render session, the closure's
# enclosing environment resolves to that session's global environment, which has
# never run tar_source("R/"). Rendering then fails with:
#
#   Error in `format_posterior_prob()`:
#   ! could not find function "format_posterior_prob"
#
# Same rule applies to any other gt helper that takes a function argument
# (fmt(), text_transform(), cols_merge() with a pattern-function, etc.).
# ---------------------------------------------------------------------------

fmt_prob <- function(x) {
  # Idempotent guard: values that have already been formatted pass through
  # untouched, so an accidental second application is harmless.
  if (is.character(x)) {
    return(x)
  }

  vapply(
    x,
    function(p) if (is.na(p)) NA_character_ else format_posterior_prob(p),
    character(1),
    USE.NAMES = FALSE
  )
}

# --- 1. Interaction Subgroup Table (Recreating "Table 3") ---
table_subgroups_function <- function(data, loc_model, study_model) {
  # Helper 1: Calculate raw counts and percentages
  get_counts <- function(df, group_var) {
    df |>
      group_by(!!sym(group_var), ich_laterality) |>
      summarize(
        n_yes = sum(neurosurgery_evac == "Yes", na.rm = TRUE),
        total = n(),
        .groups = "drop"
      ) |>
      mutate(
        pct = round(n_yes / total * 100, 1),
        val = as.character(glue::glue("{n_yes} ({pct}%)"))
      ) |>
      select(Subgroup = !!sym(group_var), ich_laterality, val) |>
      pivot_wider(names_from = ich_laterality, values_from = val)
  }

  loc_counts <- get_counts(data, "ich_location") |>
    mutate(Category = "ICH Location")
  study_counts <- get_counts(data, "study") |> mutate(Category = "Study")
  counts_df <- bind_rows(loc_counts, study_counts) |>
    mutate(Subgroup = as.character(Subgroup))

  # Helper 2: Calculate Subgroup Marginal Effects & Interactions
  get_subgroup_stats <- function(model, by_var) {
    safe_data <- as.data.frame(model$data)
    safe_data[[by_var]] <- as.factor(as.character(safe_data[[by_var]]))
    ref_level <- levels(safe_data[[by_var]])[1] # Identify baseline level

    draws <- marginaleffects::avg_comparisons(
      model,
      variables = "ich_laterality",
      by = by_var,
      newdata = safe_data,
      comparison = "lnoravg" # Extract on log-odds scale first to compute interaction
    ) |>
      marginaleffects::posterior_draws() |>
      rename(Subgroup = !!sym(by_var)) |>
      # Stable iteration ID within subgroup so pivot_wider() below cannot
      # collapse draws into list-columns.
      group_by(Subgroup) |>
      mutate(draw_iter = row_number()) |>
      ungroup()

    # 1. Main Marginal Effects (aOR within each subgroup)
    main_stats <- draws |>
      mutate(effect_ratio = exp(draw)) |>
      group_by(Subgroup) |>
      summarize(
        or = median(effect_ratio),
        lower_95_ci = quantile(effect_ratio, 0.025),
        upper_95_ci = quantile(effect_ratio, 0.975),
        or_1 = sum(effect_ratio > 1) / n(),
        or_1.2 = sum(effect_ratio > 1.2) / n(),
        rope = sum(effect_ratio < 1.05 & effect_ratio > 0.95) / n(),
        .groups = "drop"
      ) |>
      mutate(
        or_ci = as.character(glue::glue(
          "{sprintf('%.2f', or)} ({sprintf('%.2f', lower_95_ci)} - {sprintf('%.2f', upper_95_ci)})"
        )),
        Subgroup = as.character(Subgroup)
      )

    # 2. Bayesian Interaction Effects (Ratio of Odds Ratios vs Reference)
    #    .data[[ref_level]] / all_of(ref_level) are used instead of !!sym()
    #    because reference levels such as "ATACH-2" are non-syntactic names.
    interaction_stats <- draws |>
      select(draw_iter, Subgroup, draw) |>
      pivot_wider(names_from = Subgroup, values_from = draw) |>
      pivot_longer(
        cols = !all_of(c("draw_iter", ref_level)),
        names_to = "Subgroup",
        values_to = "draw_sub"
      ) |>
      mutate(
        # Difference in log-odds = Ratio of Odds Ratios
        ror = exp(draw_sub - .data[[ref_level]])
      ) |>
      group_by(Subgroup) |>
      summarize(
        ror_median = median(ror),
        ror_lower = quantile(ror, 0.025),
        ror_upper = quantile(ror, 0.975),
        prob_ror_greater = sum(ror > 1) / n(), # Posterior probability ROR > 1
        .groups = "drop"
      ) |>
      mutate(
        interaction_ci = as.character(glue::glue(
          "{sprintf('%.2f', ror_median)} ({sprintf('%.2f', ror_lower)} - {sprintf('%.2f', ror_upper)})"
        )),
        interaction_prob = fmt_prob(prob_ror_greater),
        Subgroup = as.character(Subgroup)
      ) |>
      select(Subgroup, interaction_ci, interaction_prob)

    # Add the reference level back in with placeholders
    ref_df <- tibble(
      Subgroup = ref_level,
      interaction_ci = "Reference",
      interaction_prob = "\u2014"
    )
    interaction_stats <- bind_rows(interaction_stats, ref_df)

    # Combine main and interaction stats
    main_stats |>
      left_join(interaction_stats, by = "Subgroup") |>
      select(
        Subgroup,
        or_ci,
        or_1,
        or_1.2,
        rope,
        interaction_ci,
        interaction_prob
      )
  }

  loc_stats <- get_subgroup_stats(loc_model, "ich_location")
  study_stats <- get_subgroup_stats(study_model, "study")

  # Properly bind just the two subgroup stats
  stats_df <- bind_rows(loc_stats, study_stats)

  # Combine Counts and Stats into Final gt Table.
  # fmt_prob() is applied HERE, at build time, so the gt object contains only
  # plain character strings and carries no unevaluated closures.
  final_df <- counts_df |>
    left_join(stats_df, by = "Subgroup") |>
    mutate(across(c(or_1, or_1.2, rope), fmt_prob)) |>
    select(
      Category,
      Subgroup,
      Left,
      Right,
      or_ci,
      or_1,
      or_1.2,
      rope,
      interaction_ci,
      interaction_prob
    )

  final_df |>
    gt(groupname_col = "Category") |>
    cols_label(
      Subgroup = "Subgroup",
      Left = "Left Hemisphere",
      Right = "Right Hemisphere",
      or_ci = "aOR (95% CrI)",
      or_1 = "Posterior Probability of difference (aOR > 1)",
      or_1.2 = "Posterior Probability of a substantial difference (aOR > 1.2)",
      rope = "Percentage of posterior within ROPE",
      interaction_ci = "Interaction ROR (95% CrI)",
      interaction_prob = "Probability subgroup aOR exceeds reference aOR"
    ) |>
    sub_missing(missing_text = "\u2014") |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_row_groups()
    ) |>
    tab_footnote(
      footnote = "aOR = adjusted odds ratio, CrI = 95% credible interval. Reference Category: Left Hemisphere Laterality",
      locations = cells_column_labels(columns = "or_ci")
    ) |>
    tab_footnote(
      footnote = "ROPE = region of practical equivalence, defined as 0.95 to 1.05",
      locations = cells_column_labels(columns = "rope")
    ) |>
    tab_footnote(
      footnote = "Ratio of Odds Ratios (ROR). Represents how many times larger the laterality effect (aOR) is in this subgroup compared to the Reference subgroup.",
      locations = cells_column_labels(columns = "interaction_ci")
    )
}

# --- 2. ATACH-2 Sensitivity Table (Mini Table 2) ---
table_2_atach_function <- function(
  data,
  base_model,
  site_model
) {
  n_counts <- data |>
    group_by(ich_laterality) |>
    summarize(
      n_yes = sum(neurosurgery_evac == "Yes", na.rm = TRUE),
      total = n(),
      .groups = "drop"
    ) |>
    mutate(
      pct = round(n_yes / total * 100, 1),
      val = as.character(glue::glue("{n_yes} ({pct}%)"))
    ) |>
    select(ich_laterality, val) |>
    pivot_wider(names_from = ich_laterality, values_from = val)

  get_stats <- function(model, name) {
    safe_data <- as.data.frame(model$data)
    draws <- marginaleffects::avg_comparisons(
      model,
      variables = "ich_laterality",
      newdata = safe_data,
      comparison = "lnoravg"
    ) |>
      marginaleffects::posterior_draws() |>
      mutate(draw = exp(draw))

    draws |>
      summarize(
        or = median(draw),
        lower_95_ci = quantile(draw, 0.025),
        upper_95_ci = quantile(draw, 0.975),
        or_1 = sum(draw > 1) / n(),
        or_1.2 = sum(draw > 1.2) / n(),
        rope = sum(draw < 1.05 & draw > 0.95) / n()
      ) |>
      mutate(
        Outcome = name,
        or_ci = as.character(glue::glue(
          "{sprintf('%.2f', or)} ({sprintf('%.2f', lower_95_ci)} - {sprintf('%.2f', upper_95_ci)})"
        ))
      ) |>
      select(Outcome, or_ci, or_1, or_1.2, rope)
  }

  stats_df <- bind_rows(
    get_stats(base_model, "ATACH-2 (Base Model)"),
    get_stats(site_model, "ATACH-2 (Site Random Effect)")
  ) |>
    bind_cols(n_counts) |>
    mutate(across(c(or_1, or_1.2, rope), fmt_prob)) |>
    select(Outcome, Left, Right, or_ci, or_1, or_1.2, rope)

  stats_df |>
    gt(rowname_col = "Outcome") |>
    tab_stubhead(label = "ATACH-2 Model Variants") |>
    cols_label(
      Left = "Left Hemisphere",
      Right = "Right Hemisphere",
      or_ci = "aOR (95% CrI)",
      or_1 = "Posterior Probability of difference (aOR > 1)",
      or_1.2 = "Posterior Probability of a substantial difference (aOR > 1.2)",
      rope = "Percentage of posterior within ROPE"
    ) |>
    sub_missing(missing_text = "\u2014") |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_stub(rows = everything())
    ) |>
    tab_footnote(
      footnote = "aOR = adjusted odds ratio, CrI = 95% credible interval.",
      locations = cells_column_labels(columns = "or_ci")
    )
}
