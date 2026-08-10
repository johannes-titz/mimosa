# Published worked example from Nakagawa, Johnson & Schielzeth (2017),
# Appendix S6, pp. 2-4 and 30-33:
# https://royalsocietypublishing.org/action/downloadSupplement?doi=10.1098%2Frsif.2017.0213&file=rsif20170213supp2.pdf
#
# The authors provide simulated-data code rather than a separate data file. The
# earlier simulations are retained because they advance the seeded RNG before
# the binomial example is generated.
nakagawa_2017_morph_data <- function() {
  population <- gl(12, 80, 960)
  container <- gl(120, 8, 960)
  sex <- factor(rep(rep(c("Female", "Male"), each = 8), 60))
  habitat <- factor(rep(rep(c("Dry", "Wet"), each = 4), 120))
  treatment <- factor(rep(c("Cont", "Exp"), 480))
  beetles <- data.frame(
    Population = population,
    Container = container,
    Sex = sex,
    Habitat = habitat,
    Treatment = treatment
  )

  females <- beetles[beetles$Sex == "Female", ]
  set.seed(777)

  population_effect <- stats::rnorm(12, 0, sqrt(0.4))
  container_effect <- stats::rnorm(120, 0, sqrt(0.05))
  egg_latent <- with(
    females,
    1.1 + 0.5 * (as.numeric(Treatment) - 1) +
      0.1 * (as.numeric(Habitat) - 1) +
      population_effect[Population] + container_effect[Container] +
      stats::rnorm(480, 0, sqrt(0.1))
  )
  stats::rpois(length(egg_latent), exp(egg_latent))

  population_effect <- stats::rnorm(12, 0, sqrt(0.5))
  container_effect <- stats::rnorm(120, 0, sqrt(0.8))
  parasite_latent <- with(
    beetles,
    1.8 - 2 * (as.numeric(Sex) - 1) -
      0.8 * (as.numeric(Treatment) - 1) +
      0.7 * (as.numeric(Habitat) - 1) +
      population_effect[Population] + container_effect[Container]
  )
  stats::rnbinom(length(parasite_latent), size = 5, mu = exp(parasite_latent))

  population_effect <- stats::rnorm(12, 0, sqrt(1.3))
  container_effect <- stats::rnorm(120, 0, sqrt(0.3))
  with(
    beetles,
    15 - 3 * (as.numeric(Sex) - 1) +
      0.4 * (as.numeric(Treatment) - 1) +
      0.15 * (as.numeric(Habitat) - 1) +
      population_effect[Population] + container_effect[Container] +
      stats::rnorm(960, 0, sqrt(1.2))
  )

  population_effect <- stats::rnorm(12, 0, sqrt(0.2))
  container_effect <- stats::rnorm(120, 0, sqrt(0.2))
  exploration_latent <- with(
    beetles,
    4 - (as.numeric(Sex) - 1) +
      2 * (as.numeric(Treatment) - 1) -
      0.5 * (as.numeric(Habitat) - 1) +
      population_effect[Population] + container_effect[Container]
  )
  stats::rgamma(
    length(exploration_latent),
    shape = exp(exploration_latent) * 0.3,
    rate = 0.3
  )

  males <- subset(beetles, Sex == "Male")
  population_effect <- stats::rnorm(12, 0, sqrt(1.2))
  container_effect <- stats::rnorm(120, 0, sqrt(0.2))
  colour_latent <- with(
    males,
    -0.8 + 0.8 * (as.numeric(Treatment) - 1) +
      0.5 * (as.numeric(Habitat) - 1) +
      population_effect[Population] + container_effect[Container]
  )
  males$Colour <- stats::rbinom(length(colour_latent), 1, stats::plogis(colour_latent))
  males
}

fit_nakagawa_2017_morph_model <- function() {
  lme4::glmer(
    Colour ~ Treatment + Habitat + (1 | Population) + (1 | Container),
    family = stats::binomial(link = "logit"),
    data = nakagawa_2017_morph_data()
  )
}

test_that("Nakagawa et al. (2017) binomial variance components are reproduced", {
  mdl <- fit_nakagawa_2017_morph_model()
  components <- calculate_r2_components(mdl)

  # Appendix S6 reports random variances 0.1855 and 1.1108, theoretical
  # distribution variance pi^2 / 3, R2_m = 0.04565558, and R2_c = 0.3154186.
  expect_equal(calculate_fixed_effect_variance(mdl), 0.2194053, tolerance = 1e-4)
  expect_equal(components$fixed, 0.2194053, tolerance = 1e-4)
  expect_equal(components$random, 0.1855 + 1.1108, tolerance = 5e-4)
  expect_equal(components$residual, pi^2 / 3, tolerance = 1e-6)
  expect_equal(components$marginal, 0.04565558, tolerance = 1e-4)
  expect_equal(components$conditional, 0.3154186, tolerance = 1e-4)

  # The aggregate ICC equals the sum of the two adjusted ICC components
  # reported in the paper: 0.2422109 + 0.04045751.
  aggregate_icc <- components$random / (components$random + components$residual)
  expect_equal(aggregate_icc, 0.2422109 + 0.04045751, tolerance = 1e-4)
})

test_that("published variance values appear in Mimosa calculation tooltips", {
  mdl <- fit_nakagawa_2017_morph_model()
  table <- create_table(
    mdl,
    l1 = c("Treatment", "Habitat"),
    output_options = character(0)
  )

  expect_true(grepl("0.22 + 1.30 + 3.29", table, fixed = TRUE))
  expect_true(grepl("= 0.22 / (0.22 + 1.30 + 3.29) = 0.05", table, fixed = TRUE))
  expect_true(grepl("= (0.22 + 1.30) / (0.22 + 1.30 + 3.29) = 0.32", table, fixed = TRUE))
  expect_true(grepl("= 1.30 / (1.30 + 3.29) = 0.28", table, fixed = TRUE))
})
