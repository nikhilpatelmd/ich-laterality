---
title: "Ordinal Regression - Kruscke"
code-fold: true
---

::: {.cell}

```{.r .cell-code}
library(tidyverse)
library(brms)
library(cmdstanr)
library(here)
library(ordinal)


# seeds
set.seed(160)
BAYES_SEED <- 160

# Bayes
options(
  mc.cores = parallel::detectCores(),
  brms.backend = "cmdstanr"
)
```
:::



## The Case of a Single Group

In this scenario, we will use modified Rankin scores from the ERICH dataset. First we will analyze this without any predictors.


::: {.cell}

```{.r .cell-code}
data <- read_rds(here("data", "all.rds"), refhook = NULL) |>
  drop_na(mrs_90)

erich <- data |>
  filter(study == "ERICH")

erich_plot <- erich |>
  count(mrs_90) |>
  mutate(
    prob = n / nrow(erich),
    cum_prob = cumsum(n / nrow(erich))
  )

fit <- brm(
  data = erich,
  family = cumulative(logit),
  mrs_90 ~ 1
)

fit
```

::: {.cell-output .cell-output-stdout}
```
 Family: cumulative 
  Links: mu = logit; disc = identity 
Formula: mrs_90 ~ 1 
   Data: erich (Number of observations: 2494) 
  Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
         total post-warmup draws = 4000

Population-Level Effects: 
             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
Intercept[1]    -3.03      0.09    -3.21    -2.85 1.00     2523     2509
Intercept[2]    -1.34      0.05    -1.44    -1.25 1.00     4545     3129
Intercept[3]    -0.52      0.04    -0.60    -0.44 1.00     4317     2910
Intercept[4]     0.10      0.04     0.02     0.18 1.00     4494     2959
Intercept[5]     0.93      0.05     0.84     1.02 1.00     4702     3209
Intercept[6]     1.39      0.05     1.30     1.49 1.00     4361     3265

Family Specific Parameters: 
     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
disc     1.00      0.00     1.00     1.00   NA       NA       NA

Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
and Tail_ESS are effective sample size measures, and Rhat is the potential
scale reduction factor on split chains (at convergence, Rhat = 1).
```
:::

```{.r .cell-code}
draws <- as_draws_df(fit) |>
  mutate(across(starts_with("b_"), inv_logit_scaled)) |>
  pivot_longer(starts_with("b_")) |>
  group_by(name) |>
  summarize(
    mean = mean(value),
    sd = sd(value),
    ll = quantile(value, probs = 0.025),
    up = quantile(value, probs = 0.975)
  )
```
:::

