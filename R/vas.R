vas_transformation_function <- function(model) {
# Transform VAS to 0-1 scale and fudge 0-1 to be able to use beta regression
data <- model |>
  mutate(
    euro_vas_90_transformed = euro_vas_90 / 100,
    euro_vas_180_transformed = euro_vas_180 / 100,
    euro_vas_365_transformed = euro_vas_365 / 100,
    euro_vas_90_transformed = ifelse(euro_vas_90_transformed == 0, 0.001, euro_vas_90_transformed),
    euro_vas_90_transformed = ifelse(euro_vas_90_transformed == 1, 0.999, euro_vas_90_transformed),
    euro_vas_180_transformed = ifelse(euro_vas_180_transformed == 0, 0.001, euro_vas_180_transformed),
    euro_vas_180_transformed = ifelse(euro_vas_180_transformed == 1, 0.999, euro_vas_180_transformed),    
    euro_vas_365_transformed = ifelse(euro_vas_365_transformed == 0, 0.001, euro_vas_365_transformed),
    euro_vas_365_transformed = ifelse(euro_vas_365_transformed == 1, 0.999, euro_vas_365_transformed),
  )
}

vas_plot_function <- function(data, outcome) {
  left_fill <- "#f69d75"
  right_fill <- "#551e4e"

  theme_ich <- function(base_size = 10) {
    theme_minimal(base_size = base_size) +
      theme(
        text = element_text(lineheight = 2),
        plot.title = element_text(size = rel(2), margin = margin(12, 0, 8, 0)),
        plot.subtitle = element_text(size = rel(1.1), margin = margin(4, 0, 0, 0)),
        axis.text.y = element_blank(),
        axis.title.y = element_text(
          size = rel(2),
          margin = margin(0, 40, 0, 0),
          vjust = 0.5
        ),
        axis.text.x = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5), margin = margin(10, 0, 0, 0)),
        strip.text.x = element_text(
          size = rel(2),
          margin = margin(0, 0, 20, 0),
          vjust = 0
        ),
        legend.position = "bottom",
        legend.justification = 1,
        panel.grid = element_blank(),
        plot.caption = element_text(size = rel(1), margin = margin(8, 0, 0, 0)),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
      )
  }

  # VAS Plot
  vas_plot <- data |>
    ggplot(
      aes(x = {{ outcome }}, fill = ich_laterality)) +
        geom_density(alpha = 0.7, linewidth = 0.1) +
        scale_fill_manual(values = c(left_fill, right_fill)) +
        scale_x_continuous(
          limits = c(0, 100),
          breaks = seq(0, 100, 10)
        ) +
        labs(
          x = NULL,
          y = "Density",
          fill = "Hemispheric Laterality"
        ) +
    theme_ich()

  return(vas_plot)
}

f_vas_90_beta <- function(data) {

  settings <- model_setup()

  model <- brm(
    bf(euro_vas_90_transformed ~ ich_laterality + age + ich_volume_baseline + gcs_baseline + ivh + ich_location,
      phi ~ euro_vas_90_transformed),
    data = data,
    family = Beta(),
    # sample_prior = "only",
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
    )
  
    return(model)
}

f_vas_180_beta <- function(data) {

  settings <- model_setup()

  model <- brm(
    bf(euro_vas_180_transformed ~ ich_laterality + age + ich_volume_baseline + gcs_baseline + ivh + ich_location,
      phi ~ euro_vas_180_transformed),
    data = data,
    family = Beta(),
    # sample_prior = "only",
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
    )
  
    return(model)
}

f_vas_365_beta <- function(data) {

  settings <- model_setup()

  model <- brm(
    bf(euro_vas_365_transformed ~ ich_laterality + age + ich_volume_baseline + gcs_baseline + ivh + ich_location,
      phi ~ euro_vas_365_transformed),
    data = data,
    family = Beta(),
    # sample_prior = "only",
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
    )
  
    return(model)
}