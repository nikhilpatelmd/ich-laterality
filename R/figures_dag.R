# R/figures_dag.R
# DAG visualization functions for supplement figures.
# Produces two figures: the neurosurgery DAG and the functional outcomes DAG.

# ---------------------------------------------------------------------------
# Shared aesthetic constants
# Consistent with manuscript Okabe-Ito palette and cairo_pdf conventions.
# ---------------------------------------------------------------------------

DAG_PALETTE <- c(
  "exposure" = "#0072B2", # Okabe-Ito blue   — hemispheric laterality
  "outcome" = "#D55E00", # Okabe-Ito orange — outcome node
  "latent" = "#BBBBBB", # gray             — unmeasured / latent
  "covariate" = "#E8E8E8" # light gray        — measured covariate
)

# Slightly darker colors for node borders (stroke), derived manually to avoid
# a colorspace dependency.
DAG_BORDER <- c(
  "exposure" = "#004F80",
  "outcome" = "#9A4200",
  "latent" = "#888888",
  "covariate" = "#AAAAAA"
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

    # Edges drawn first so they appear behind node circles.
    # geom_dag_edges_arc() handles curved edges cleanly when many paths
    # run between the same region of the graph.
    geom_dag_edges_arc(
      curvature = edge_curvature,
      edge_colour = "grey45",
      edge_width = 0.5,
      arrow = grid::arrow(length = unit(0.12, "cm"), type = "closed")
    ) +

    # shape = 21 gives a filled circle with a separate stroke color.
    geom_dag_point(
      aes(fill = node_type, color = node_type),
      size = 16,
      shape = 21,
      stroke = 1.1
    ) +

    # Label box fill matches node color, visually linking label to node
    # even when repelled away from it.
    geom_dag_label_repel(
      aes(label = label, fill = node_type),
      color = "grey10",
      size = base_size * 0.22,
      label.padding = unit(0.12, "cm"),
      box.padding = unit(0.4, "cm"),
      max.overlaps = 20,
      show.legend = FALSE,
      seed = 42 # reproducible repulsion layout
    ) +

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
# Figure assembly functions.
# Each accepts a pre-built dagitty object as its argument so that targets
# can register it as an explicit upstream dependency and invalidate the
# figure target whenever the DAG definition changes.
# ---------------------------------------------------------------------------

make_neurosurgery_dag_figure <- function(dag) {
  plot_dag(
    dag,
    title = "Neurosurgical Intervention",
    subtitle = "Directed acyclic graph for the primary surgical outcome"
  )
}

make_outcomes_dag_figure <- function(dag) {
  plot_dag(
    dag,
    title = "Functional Outcomes",
    subtitle = "Directed acyclic graph for mRS and EuroQOL outcomes"
  )
}

# ---------------------------------------------------------------------------
# Standalone export helper (interactive / non-pipeline use).
# In the targets pipeline, ggsave is called inside the file targets in
# _targets.R instead; this function is provided for convenience only.
# ---------------------------------------------------------------------------

save_dag_figures <- function(output_dir = "figures/supplement") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  neuro_dag <- f_neurosurgery_dag()
  outcomes_dag <- outcomes_dag_function()

  ggsave(
    filename = file.path(output_dir, "sfig_dag_neurosurgery.pdf"),
    plot = make_neurosurgery_dag_figure(neuro_dag),
    device = cairo_pdf,
    width = 9,
    height = 9,
    units = "in"
  )

  ggsave(
    filename = file.path(output_dir, "sfig_dag_outcomes.pdf"),
    plot = make_outcomes_dag_figure(outcomes_dag),
    device = cairo_pdf,
    width = 11,
    height = 10,
    units = "in"
  )

  invisible(list(
    neurosurgery = make_neurosurgery_dag_figure(neuro_dag),
    outcomes = make_outcomes_dag_figure(outcomes_dag)
  ))
}
