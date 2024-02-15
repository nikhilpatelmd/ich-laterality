library(purrr)
library(repurrrsive)

prior_f <- function(formula, priors, data, family) {
  mod_prior <- brm(formula,
    data = data,
    family = family,
    prior = priors,
    sample_prior = "only"
  )
  return(mod_prior)
}

prior_arguments <- list(
  formula = neurosurgery_evac ~ ich_laterality,
  priors = c(
    set_prior("normal(-2.2, 0.7)", class = "Intercept"),
    set_prior("normal(0, 1)", class = "b")
  ),
  data = ich_aggressive,
  family = bernoulli(link = "logit")
)

prior_f(prior_arguments$formula, prior_arguments$priors, prior_arguments$data, prior_arguments$family)

wesanderson


## purrr tutorial

map(prior_arguments, "formula")

map(got_chars[5:9], 1)
map(wesanderson, 1)
map(prior_arguments, 1)

got_chars |>
  map(3)

names(got_chars)
str(got_chars)

map_lgl(got_chars, "alive")
map_chr(got_chars, as.character("id"))

culture <- factor(map_vec(got_chars, "culture"))
unique(culture)

map(wesanderson$name, print)

tibble <- names(wesanderson)

map_chr(wesanderson, 1)

wes_tibble <- tibble(
  movie = names(wesanderson),
  color1 = map_chr(wesanderson, 1),
  color2 = map_chr(wesanderson, 2)
)

map(shades, print)

colors


models <- list(
  simple = "lifeexp ~ pop + gdppercap",
  medium = "lifeexp ~ pop + gdppercap + continent + year",
  more = "lifeexp ~ pop + gdppercap + country + year",
  woah = "lifeexp ~ pop + gdppercap + year*country"
)

model_frame <- data_frame(model = models) %>%
  mutate(model_name = names(model))

model_frame <- model_frame %>%
  mutate(model = map(model, as.formula))

model_frame

prior_formulas <- list(

)


lhr_setup <- function() {
  # Settings
  CHAINS <- 4
  ITER <- 2000
  WARMUP <- 1000
  BAYES_SEED <- 4045 # From random.org
  threads <- getOption("mc.threads")

  # Priors
  priors_vague <- c(
    set_prior("normal(0, 10)", class = "Intercept"),
    set_prior("normal(0, 3)", class = "b"),
    set_prior("cauchy(0, 1)", class = "sd")
  )

  return(list(
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
    threads = threads, priors_vague = priors_vague
  ))
}

lhr <- lhr_setup()
