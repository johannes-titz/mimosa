# Published Gaussian worked example from Nakagawa & Schielzeth (2013), Table 3
# and supporting Data S1/Data S4:
# https://doi.org/10.1111/j.2041-210x.2012.00261.x
#
# The packaged BeetlesBody data are maintained by the authors in the
# MIT-licensed rptR package and sourced from:
# https://github.com/mastoffel/rptR/blob/master/data/BeetlesBody.rda
fit_nakagawa_2013_size_model <- function() {
  beetles <- load_package_dataset("BeetlesBody")
  lme4::lmer(
    BodyL ~ Sex + Treatment + Habitat +
      (1 | Population) + (1 | Container),
    data = beetles
  )
}

test_that("BeetlesBody is included as a documented Mimosa data set", {
  beetles <- load_package_dataset("BeetlesBody")

  expect_s3_class(beetles, "data.frame")
  expect_equal(dim(beetles), c(960, 6))
  expect_identical(
    names(beetles),
    c("Population", "Container", "Sex", "Habitat", "Treatment", "BodyL")
  )
})

test_that("Nakagawa and Schielzeth (2013) Gaussian components are reproduced", {
  mdl <- fit_nakagawa_2013_size_model()
  components <- calculate_r2_components(mdl)

  expect_s4_class(mdl, "lmerMod")
  expect_equal(calculate_fixed_effect_variance(mdl), 1.81, tolerance = 5e-3)
  expect_equal(components$fixed, 1.81, tolerance = 5e-3)
  expect_equal(components$random, 0.23 + 1.38, tolerance = 1e-2)
  expect_equal(components$residual, 1.20, tolerance = 5e-3)
  expect_equal(components$marginal, 0.39, tolerance = 5e-3)
  expect_equal(components$conditional, 0.74, tolerance = 5e-3)
})

test_that("published Gaussian values appear in Mimosa calculation tooltips", {
  mdl <- fit_nakagawa_2013_size_model()
  table <- create_table(
    mdl,
    l1 = c("Sex", "Treatment", "Habitat"),
    output_options = character(0)
  )

  expect_true(grepl("1.81 + 1.61 + 1.20", table, fixed = TRUE))
  expect_true(grepl("= 1.81 / (1.81 + 1.61 + 1.20) = 0.39", table, fixed = TRUE))
  expect_true(grepl("= (1.81 + 1.61) / (1.81 + 1.61 + 1.20) = 0.74", table, fixed = TRUE))
  expect_true(grepl("= 1.61 / (1.61 + 1.20) = 0.57", table, fixed = TRUE))
})
