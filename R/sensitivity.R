# Alternative Adjustment Sets

f_posterior_minimal_neurosurgery <- function(dat) {
  settings <- model_setup()

  model <- brm(
    neurosurgery_evac ~
      ich_laterality +
        (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-7, 0.35)", class = "Intercept"),
      set_prior("normal(0, 0.5)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    control = list(adapt_delta = 0.99)
  )

  return(model)
}

f_posterior_alternative1_neurosurgery <- function(dat) {
  settings <- model_setup()

  model <- brm(
    neurosurgery_evac ~
      ich_laterality +
        ich_volume_baseline +
        (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-7, 0.35)", class = "Intercept"),
      set_prior("normal(0, 0.5)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    control = list(adapt_delta = 0.99)
  )

  return(model)
}

f_posterior_alternative2_neurosurgery <- function(dat) {
  settings <- model_setup()

  model <- brm(
    neurosurgery_evac ~
      ich_laterality +
        ich_volume_baseline +
        age +
        (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-7, 0.35)", class = "Intercept"),
      set_prior("normal(0, 0.5)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    control = list(adapt_delta = 0.99)
  )

  return(model)
}

f_posterior_alternative3_neurosurgery <- function(dat) {
  settings <- model_setup()

  model <- brm(
    neurosurgery_evac ~
      ich_laterality +
        ich_volume_baseline +
        age +
        ich_location +
        (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-7, 0.35)", class = "Intercept"),
      set_prior("normal(0, 0.5)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    control = list(adapt_delta = 0.99)
  )

  return(model)
}

f_posterior_alternative4_neurosurgery <- function(dat) {
  settings <- model_setup()

  model <- brm(
    neurosurgery_evac ~
      ich_laterality +
        ich_volume_baseline +
        age +
        ich_location +
        gcs_baseline +
        (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-7, 0.35)", class = "Intercept"),
      set_prior("normal(0, 0.5)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    control = list(adapt_delta = 0.99)
  )

  return(model)
}

f_posterior_minimal_evd <- function(dat) {
  settings <- model_setup()

  model <- brm(
    evd ~
      ich_laterality +
        (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-7, 0.35)", class = "Intercept"),
      set_prior("normal(0, 0.5)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    control = list(adapt_delta = 0.99)
  )

  return(model)
}

f_posterior_alternative1_evd <- function(dat) {
  settings <- model_setup()

  model <- brm(
    evd ~
      ich_laterality +
        ich_volume_baseline +
        (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-7, 0.35)", class = "Intercept"),
      set_prior("normal(0, 0.5)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    control = list(adapt_delta = 0.99)
  )

  return(model)
}

f_posterior_alternative2_evd <- function(dat) {
  settings <- model_setup()

  model <- brm(
    evd ~
      ich_laterality +
        ich_volume_baseline +
        age +
        (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-7, 0.35)", class = "Intercept"),
      set_prior("normal(0, 0.5)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    control = list(adapt_delta = 0.99)
  )

  return(model)
}

f_posterior_alternative3_evd <- function(dat) {
  settings <- model_setup()

  model <- brm(
    evd ~
      ich_laterality +
        ich_volume_baseline +
        age +
        ich_location +
        (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-7, 0.35)", class = "Intercept"),
      set_prior("normal(0, 0.5)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    control = list(adapt_delta = 0.99)
  )

  return(model)
}

f_posterior_alternative4_evd <- function(dat) {
  settings <- model_setup()

  model <- brm(
    evd ~
      ich_laterality +
        ich_volume_baseline +
        age +
        ich_location +
        gcs_baseline +
        (1 | study),
    family = bernoulli(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-7, 0.35)", class = "Intercept"),
      set_prior("normal(0, 0.5)", class = "b")
    ),
    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed,
    control = list(adapt_delta = 0.99)
  )

  return(model)
}

f_posterior_alternative1_mrs_90 <- function(dat) {
  settings <- model_setup()

  model <- brm(
    mrs_90 ~
      ich_laterality +
        ich_volume_baseline +
        (1 | study),
    family = cumulative(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),

    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_posterior_alternative2_mrs_90 <- function(dat) {
  settings <- model_setup()

  model <- brm(
    mrs_90 ~
      ich_laterality +
        age +
        ich_volume_baseline +
        (1 | study),
    family = cumulative(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),

    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_posterior_alternative3_mrs_90 <- function(dat) {
  settings <- model_setup()

  model <- brm(
    mrs_90 ~
      ich_laterality +
        age +
        ich_volume_baseline *
          ich_location +
        (1 | study),
    family = cumulative(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),

    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_posterior_alternative4_mrs_90 <- function(dat) {
  settings <- model_setup()

  model <- brm(
    mrs_90 ~
      ich_laterality +
        age +
        ich_volume_baseline *
          ich_location +
        gcs_baseline +
        (1 | study),
    family = cumulative(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),

    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_posterior_alternative5_mrs_90 <- function(dat) {
  settings <- model_setup()

  model <- brm(
    mrs_90 ~
      ich_laterality +
        age +
        ich_volume_baseline *
          ich_location +
        gcs_baseline +
        ivh +
        (1 | study),
    family = cumulative(link = "logit"),
    data = dat,
    prior = c(
      set_prior("normal(-2.2, 0.5)", class = "Intercept"),
      set_prior("normal(0, 1)", class = "b")
    ),

    cores = settings$cores,
    chains = settings$chains,
    threads = settings$threads,
    warmup = settings$warmup,
    iter = settings$iter,
    seed = settings$seed
  )

  return(model)
}

f_alternative_adjustment_table_mrs <- function(m1, m2, m3, m4, m5, m6) {
  results1 <- m1 |>
    spread_draws(b_ich_lateralityRight) |>
    mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
    summarize(
      or = median(ich_right_or),
      lower_95_ci = quantile(ich_right_or, 0.025),
      upper_95_ci = quantile(ich_right_or, 0.975),
      or_1 = sum(ich_right_or > 1) / n(),
      or_1.1 = sum(ich_right_or > 1.1) / n(),
      or_1.2 = sum(ich_right_or > 1.2) / n(),
      rope = sum(ich_right_or < 1.05 & ich_right_or > 0.95) / n()
    ) |>
    mutate(
      or_ci = glue(
        "{round(or, digits = 2)} ({round(lower_95_ci, digits = 2)} - {round(upper_95_ci, digits = 2)})"
      )
    ) |>
    select(or_ci, or_1, or_1.2, rope)

  results2 <- m2 |>
    spread_draws(b_ich_lateralityRight) |>
    mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
    summarize(
      or = median(ich_right_or),
      lower_95_ci = quantile(ich_right_or, 0.025),
      upper_95_ci = quantile(ich_right_or, 0.975),
      or_1 = sum(ich_right_or > 1) / n(),
      or_1.1 = sum(ich_right_or > 1.1) / n(),
      or_1.2 = sum(ich_right_or > 1.2) / n(),
      rope = sum(ich_right_or < 1.05 & ich_right_or > 0.95) / n()
    ) |>
    mutate(
      or_ci = glue(
        "{round(or, digits = 2)} ({round(lower_95_ci, digits = 2)} - {round(upper_95_ci, digits = 2)})"
      )
    ) |>
    select(or_ci, or_1, or_1.2, rope)

  results3 <- m3 |>
    spread_draws(b_ich_lateralityRight) |>
    mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
    summarize(
      or = median(ich_right_or),
      lower_95_ci = quantile(ich_right_or, 0.025),
      upper_95_ci = quantile(ich_right_or, 0.975),
      or_1 = sum(ich_right_or > 1) / n(),
      or_1.1 = sum(ich_right_or > 1.1) / n(),
      or_1.2 = sum(ich_right_or > 1.2) / n(),
      rope = sum(ich_right_or < 1.05 & ich_right_or > 0.95) / n()
    ) |>
    mutate(
      or_ci = glue(
        "{round(or, digits = 2)} ({round(lower_95_ci, digits = 2)} - {round(upper_95_ci, digits = 2)})"
      )
    ) |>
    select(or_ci, or_1, or_1.2, rope)

  results4 <- m4 |>
    spread_draws(b_ich_lateralityRight) |>
    mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
    summarize(
      or = median(ich_right_or),
      lower_95_ci = quantile(ich_right_or, 0.025),
      upper_95_ci = quantile(ich_right_or, 0.975),
      or_1 = sum(ich_right_or > 1) / n(),
      or_1.1 = sum(ich_right_or > 1.1) / n(),
      or_1.2 = sum(ich_right_or > 1.2) / n(),
      rope = sum(ich_right_or < 1.05 & ich_right_or > 0.95) / n()
    ) |>
    mutate(
      or_ci = glue(
        "{round(or, digits = 2)} ({round(lower_95_ci, digits = 2)} - {round(upper_95_ci, digits = 2)})"
      )
    ) |>
    select(or_ci, or_1, or_1.2, rope)

  results5 <- m5 |>
    spread_draws(b_ich_lateralityRight) |>
    mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
    summarize(
      or = median(ich_right_or),
      lower_95_ci = quantile(ich_right_or, 0.025),
      upper_95_ci = quantile(ich_right_or, 0.975),
      or_1 = sum(ich_right_or > 1) / n(),
      or_1.1 = sum(ich_right_or > 1.1) / n(),
      or_1.2 = sum(ich_right_or > 1.2) / n(),
      rope = sum(ich_right_or < 1.05 & ich_right_or > 0.95) / n()
    ) |>
    mutate(
      or_ci = glue(
        "{round(or, digits = 2)} ({round(lower_95_ci, digits = 2)} - {round(upper_95_ci, digits = 2)})"
      )
    ) |>
    select(or_ci, or_1, or_1.2, rope)

  results6 <- m6 |>
    spread_draws(b_ich_lateralityRight) |>
    mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
    summarize(
      or = median(ich_right_or),
      lower_95_ci = quantile(ich_right_or, 0.025),
      upper_95_ci = quantile(ich_right_or, 0.975),
      or_1 = sum(ich_right_or > 1) / n(),
      or_1.1 = sum(ich_right_or > 1.1) / n(),
      or_1.2 = sum(ich_right_or > 1.2) / n(),
      rope = sum(ich_right_or < 1.05 & ich_right_or > 0.95) / n()
    ) |>
    mutate(
      or_ci = glue(
        "{round(or, digits = 2)} ({round(lower_95_ci, digits = 2)} - {round(upper_95_ci, digits = 2)})"
      )
    ) |>
    select(or_ci, or_1, or_1.2, rope)

  table <- bind_rows(
    "Minimal Model" = results1,
    "Alternative Model 1" = results2,
    "Alternative Model 2" = results3,
    "Alternative Model 3" = results4,
    "Alternative Model 4" = results5,
    "Alternative Model 5" = results6,
    .id = "Model"
  ) |>
    gt() |>
    tab_stubhead(label = "Model") |>
    cols_label(
      Model = md("**Model**"),
      or_ci = md("**aOR (95% CI)**"),
      or_1 = md("**Probability of any difference (aOR > 1)**"),
      or_1.2 = md("**Probability of a substantial difference (aOR > 1.2)**"),
      rope = md("**ROPE**")
    ) |>
    fmt_number(columns = 2:4, decimals = 2) |>
    cols_width(
      Model ~ px(375),
      2 ~ px(150),
      3:5 ~ px(125)
    ) |>
    cols_align(align = "left") |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_stub(rows = everything())
    ) |>
    tab_footnote(
      footnote = "aOR = adjusted odds ratio, CI = 95% credible interval; adjusted for age, admission GCS, ICH location, ICH volume, IVH, and study (as random intercept)",
      locations = cells_column_labels(columns = or_ci)
    ) |>
    tab_footnote(
      footnote = "ROPE = region of practical equivalence, defined as 0.95 > aOR > 1.05",
      locations = cells_column_labels(columns = rope)
    )
}

f_alternative_adjustment_table <- function(m1, m2, m3, m4, m5) {
  results1 <- m1 |>
    spread_draws(b_ich_lateralityRight) |>
    mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
    summarize(
      or = median(ich_right_or),
      lower_95_ci = quantile(ich_right_or, 0.025),
      upper_95_ci = quantile(ich_right_or, 0.975),
      or_1 = sum(ich_right_or > 1) / n(),
      or_1.1 = sum(ich_right_or > 1.1) / n(),
      or_1.2 = sum(ich_right_or > 1.2) / n(),
      rope = sum(ich_right_or < 1.05 & ich_right_or > 0.95) / n()
    ) |>
    mutate(
      or_ci = glue(
        "{round(or, digits = 2)} ({round(lower_95_ci, digits = 2)} - {round(upper_95_ci, digits = 2)})"
      )
    ) |>
    select(or_ci, or_1, or_1.2, rope)

  results2 <- m2 |>
    spread_draws(b_ich_lateralityRight) |>
    mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
    summarize(
      or = median(ich_right_or),
      lower_95_ci = quantile(ich_right_or, 0.025),
      upper_95_ci = quantile(ich_right_or, 0.975),
      or_1 = sum(ich_right_or > 1) / n(),
      or_1.1 = sum(ich_right_or > 1.1) / n(),
      or_1.2 = sum(ich_right_or > 1.2) / n(),
      rope = sum(ich_right_or < 1.05 & ich_right_or > 0.95) / n()
    ) |>
    mutate(
      or_ci = glue(
        "{round(or, digits = 2)} ({round(lower_95_ci, digits = 2)} - {round(upper_95_ci, digits = 2)})"
      )
    ) |>
    select(or_ci, or_1, or_1.2, rope)

  results3 <- m3 |>
    spread_draws(b_ich_lateralityRight) |>
    mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
    summarize(
      or = median(ich_right_or),
      lower_95_ci = quantile(ich_right_or, 0.025),
      upper_95_ci = quantile(ich_right_or, 0.975),
      or_1 = sum(ich_right_or > 1) / n(),
      or_1.1 = sum(ich_right_or > 1.1) / n(),
      or_1.2 = sum(ich_right_or > 1.2) / n(),
      rope = sum(ich_right_or < 1.05 & ich_right_or > 0.95) / n()
    ) |>
    mutate(
      or_ci = glue(
        "{round(or, digits = 2)} ({round(lower_95_ci, digits = 2)} - {round(upper_95_ci, digits = 2)})"
      )
    ) |>
    select(or_ci, or_1, or_1.2, rope)

  results4 <- m4 |>
    spread_draws(b_ich_lateralityRight) |>
    mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
    summarize(
      or = median(ich_right_or),
      lower_95_ci = quantile(ich_right_or, 0.025),
      upper_95_ci = quantile(ich_right_or, 0.975),
      or_1 = sum(ich_right_or > 1) / n(),
      or_1.1 = sum(ich_right_or > 1.1) / n(),
      or_1.2 = sum(ich_right_or > 1.2) / n(),
      rope = sum(ich_right_or < 1.05 & ich_right_or > 0.95) / n()
    ) |>
    mutate(
      or_ci = glue(
        "{round(or, digits = 2)} ({round(lower_95_ci, digits = 2)} - {round(upper_95_ci, digits = 2)})"
      )
    ) |>
    select(or_ci, or_1, or_1.2, rope)

  results5 <- m5 |>
    spread_draws(b_ich_lateralityRight) |>
    mutate(ich_right_or = exp(b_ich_lateralityRight)) |>
    summarize(
      or = median(ich_right_or),
      lower_95_ci = quantile(ich_right_or, 0.025),
      upper_95_ci = quantile(ich_right_or, 0.975),
      or_1 = sum(ich_right_or > 1) / n(),
      or_1.1 = sum(ich_right_or > 1.1) / n(),
      or_1.2 = sum(ich_right_or > 1.2) / n(),
      rope = sum(ich_right_or < 1.05 & ich_right_or > 0.95) / n()
    ) |>
    mutate(
      or_ci = glue(
        "{round(or, digits = 2)} ({round(lower_95_ci, digits = 2)} - {round(upper_95_ci, digits = 2)})"
      )
    ) |>
    select(or_ci, or_1, or_1.2, rope)

  table <- bind_rows(
    "Minimal Model" = results1,
    "Alternative Model 1" = results2,
    "Alternative Model 2" = results3,
    "Alternative Model 3" = results4,
    "Alternative Model 4" = results5,
    .id = "Model"
  ) |>
    gt() |>
    tab_stubhead(label = "Model") |>
    cols_label(
      Model = md("**Model**"),
      or_ci = md("**aOR (95% CI)**"),
      or_1 = md("**Probability of any difference (aOR > 1)**"),
      or_1.2 = md("**Probability of a substantial difference (aOR > 1.2)**"),
      rope = md("**ROPE**")
    ) |>
    fmt_number(columns = 2:5, decimals = 2) |>
    cols_width(
      Model ~ px(375),
      2 ~ px(150),
      3:5 ~ px(125)
    ) |>
    cols_align(align = "left") |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_stub(rows = everything())
    ) |>
    tab_footnote(
      footnote = "aOR = adjusted odds ratio, CI = 95% credible interval; adjusted for age, admission GCS, ICH location, ICH volume, IVH, and study (as random intercept)",
      locations = cells_column_labels(columns = or_ci)
    ) |>
    tab_footnote(
      footnote = "ROPE = region of practical equivalence, defined as 0.95 > aOR > 1.05",
      locations = cells_column_labels(columns = rope)
    )
}
