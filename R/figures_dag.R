# R/figures_dag.R
# DAG visualization functions for supplement figures.
# Produces two figures: the neurosurgery DAG and the functional outcomes DAG,
# each with a companion adjustment-set panel.



# ---------------------------------------------------------------------------
# Shared aesthetic constants
# Consistent with manuscript Okabe-Ito palette and cairo_pdf conventions.
# ---------------------------------------------------------------------------

DAG_PALETTE <- c(
  "exposure" = "#0072B2", # Okabe-Ito blue  — hemispheric laterality
  "outcome" = "#D55E00", # Okabe-Ito orange — outcome node
  "latent" = "#BBBBBB", # gray             — unmeasured / latent
  "covariate" = "#E8E8E8" # light gray       — measured covariate
)

# Slightly darker colors for node borders (stroke), derived manually to avoid
# a colorspace dependency.
DAG_BORDER <- c(
  "exposure" = "#004F80",
  "outcome" = "#9A4200",
  "latent" = "#888888",
  "covariate" = "#AAAAAA"
)

# Text colors: white on dark fills, dark on light fills.
DAG_TEXT <- c(
  "exposure" = "white",
  "outcome" = "white",
  "latent" = "#444444",
  "covariate" = "#333333"
)

# ---------------------------------------------------------------------------
# Helper: classify each node as exposure / outcome / latent / covariate.
# Works on the tidy_dagitty data frame by cross-referencing the dagitty object.
# ---------------------------------------------------------------------------

annotate_node_types <- function(tidy_dag, dag_obj) {
  tidy_dag |>
    mutate(
      node_type = case_when(
        name %in% exposures(dag_obj) ~ "exposure",
        name %in% outcomes(dag_obj) ~ "outcome",
        name %in% latents(dag_obj) ~ "latent",
        TRUE ~ "covariate"
      ),
      # Factor with explicit ordering controls legend order.
      node_type = factor(
        node_type,
        levels = c("exposure", "outcome", "latent", "covariate")
      )
    )
}

# ---------------------------------------------------------------------------
# Core DAG plotting function.
# Returns a ggplot object; title and subtitle are passed through labs().
# edge_curvature: small positive value prevents straight edges from perfectly
# overlapping when two nodes share multiple paths.
# ---------------------------------------------------------------------------

plot_dag <- function(
  dag_obj,
  title = NULL,
  subtitle = NULL,
  edge_curvature = 0.15,
  base_size = 11
) {
  tidy_dag <- tidy_dagitty(dag_obj) |>
    annotate_node_types(dag_obj)

  ggplot(tidy_dag, aes(x = x, y = y, xend = xend, yend = yend)) +

    # --- Edges (drawn first, so they appear behind node circles) ---
    # geom_dag_edges_arc() handles curved edges cleanly when many paths
    # run between the same region of the graph.
    geom_dag_edges_arc(
      curvature = edge_curvature,
      edge_colour = "grey45",
      edge_width = 0.5,
      arrow = grid::arrow(
        length = unit(0.12, "cm"),
        type = "closed"
      )
    ) +

    # --- Node circles ---
    # shape = 21 gives a filled circle with a separate stroke color,
    # which is how we get the two-tone exposure/outcome appearance.
    geom_dag_point(
      aes(fill = node_type, color = node_type),
      size = 16,
      shape = 21,
      stroke = 1.1
    ) +

    # --- Node labels (repelled to reduce overlap with long label strings) ---
    # We pass fill through aes() so the label box matches the node color —
    # this visually "connects" label to node even when repelled far away.
    geom_dag_label_repel(
      aes(label = label, fill = node_type),
      color = "grey10",
      size = base_size * 0.22, # scales with base_size
      label.padding = unit(0.12, "cm"),
      box.padding = unit(0.4, "cm"),
      max.overlaps = 20,
      show.legend = FALSE,
      seed = 42 # reproducible repulsion layout
    ) +

    # --- Scales ---
    scale_fill_manual(
      values = DAG_PALETTE,
      labels = c("Exposure", "Outcome", "Unmeasured", "Covariate"),
      name = NULL,
      drop = FALSE
    ) +
    scale_color_manual(
      values = DAG_BORDER,
      guide = "none",
      drop = FALSE
    ) +

    # --- Theme ---
    theme_dag(base_size = base_size) +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = base_size * 0.85),
      plot.title = element_text(face = "bold", size = base_size * 1.1),
      plot.subtitle = element_text(size = base_size * 0.9, color = "grey30"),
      plot.caption = element_text(
        size = base_size * 0.75,
        color = "grey40",
        hjust = 0
      )
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      caption = "Gray nodes indicate unmeasured (latent) variables."
    )
}

# ---------------------------------------------------------------------------
# Adjustment set visualization.
# Uses ggdag::ggdag_adjustment_set() as the base, then re-styles to match
# plot_dag(). Shows which covariates must be conditioned on for each
# minimal valid adjustment set.
# ---------------------------------------------------------------------------

plot_adjustment_set <- function(
  dag_obj,
  title = "Minimal Adjustment Sets",
  base_size = 11
) {
  # ggdag_adjustment_set() facets by adjustment set and colors nodes by role
  # (exposure, outcome, adjusted, unadjusted). We override its color scale.
  ggdag_adjustment_set(
    dag_obj,
    node_size = 14,
    text = FALSE, # we'll add our own labels via geom_dag_label_repel
    use_labels = "label",
    shadow = TRUE
  ) +
    scale_fill_manual(
      values = c(
        "exposure" = "#0072B2",
        "outcome" = "#D55E00",
        "adjusted" = "#009E73", # Okabe-Ito green = in adjustment set
        "unadjusted" = "#E8E8E8"
      ),
      name = NULL
    ) +
    theme_dag(base_size = base_size) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = base_size * 0.85)
    ) +
    labs(title = title)
}

# ---------------------------------------------------------------------------
# Figure assembly functions.
# Each returns a patchwork-composed plot ready for ggsave().
# ---------------------------------------------------------------------------

make_neurosurgery_dag_figure <- function() {
  dag_obj <- f_neurosurgery_dag()

  main_panel <- plot_dag(
    dag_obj,
    title = "A  Neurosurgical Intervention",
    subtitle = "Directed acyclic graph for the primary surgical outcome"
  )

  adj_panel <- plot_adjustment_set(
    dag_obj,
    title = "B  Minimal adjustment sets"
  )

  # Stack vertically; main DAG gets more height than adjustment set panel.
  main_panel /
    adj_panel +
    plot_layout(heights = c(2, 1)) &
    theme(plot.margin = margin(8, 8, 8, 8))
}


make_outcomes_dag_figure <- function() {
  dag_obj <- outcomes_dag_function()

  main_panel <- plot_dag(
    dag_obj,
    title = "A  Functional Outcomes",
    subtitle = "Directed acyclic graph for mRS and EuroQOL outcomes"
  )

  adj_panel <- plot_adjustment_set(
    dag_obj,
    title = "B  Minimal adjustment sets"
  )

  main_panel /
    adj_panel +
    plot_layout(heights = c(2, 1)) &
    theme(plot.margin = margin(8, 8, 8, 8))
}

# ---------------------------------------------------------------------------
# Export function.
# cairo_pdf preserves text as proper glyphs and handles Unicode correctly —
# important if any labels contain special characters.
# Dimensions are set wide to give the outcome DAG (more nodes) room to breathe.
# ---------------------------------------------------------------------------

save_dag_figures <- function(output_dir = "figures/supplement") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  base_pt <- 14
  base_mm <- base_pt / 2.835 # convert pt to mm for ggplot2 base_size

  ggsave(
    filename = file.path(output_dir, "sfig_dag_neurosurgery.pdf"),
    plot = make_neurosurgery_dag_figure(),
    device = cairo_pdf,
    width = 9,
    height = 9,
    units = "in"
  )

  # The outcomes DAG has more nodes and two latent social variables,
  # so it gets extra width.
  ggsave(
    filename = file.path(output_dir, "sfig_dag_outcomes.pdf"),
    plot = make_outcomes_dag_figure(),
    device = cairo_pdf,
    width = 11,
    height = 10,
    units = "in"
  )

  invisible(list(
    neurosurgery = make_neurosurgery_dag_figure(),
    outcomes = make_outcomes_dag_figure()
  ))
}
