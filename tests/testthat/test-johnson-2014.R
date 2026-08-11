# Peer-reviewed two-level Gaussian worked example from Johnson (2014),
# DOI 10.1111/2041-210X.12225, and its Data S1 calculation script:
# https://pmc.ncbi.nlm.nih.gov/articles/PMC4368045/
#
# The data contain seven circumference measurements nested within each of five
# orange trees. Data S1 fits both random-slope and random-intercept models. The
# random-intercept comparison is used here because it is non-singular and can
# be reproduced directly in Mimosa; Data S1 reports R2_m = 0.82 and R2_c = 0.93.
fit_johnson_2014_orange_model <- function() {
  orange <- transform(datasets::Orange, ageYears = age / 365.25)
  lme4::lmer(
    circumference ~ ageYears + (1 | Tree),
    data = orange
  )
}

# Direct implementation of Johnson's Data S1 calculations: fixed variance
# var(X beta), mean random variance from equation 11, and Gaussian residual
# variance. This deliberately does not call insight::get_variance().
calculate_johnson_2014_components <- function(mdl) {
  fixed_design <- stats::model.matrix(mdl)
  fixed <- stats::var(fixed_design %*% lme4::fixef(mdl))
  random_covariances <- lme4::VarCorr(mdl)
  n <- nrow(fixed_design)
  random <- sum(vapply(
    random_covariances,
    function(covariance) {
      random_design <- fixed_design[, rownames(covariance), drop = FALSE]
      sum(diag(random_design %*% covariance %*% t(random_design))) / n
    },
    numeric(1)
  ))
  residual <- attr(random_covariances, "sc")^2
  total <- fixed + random + residual
  list(
    fixed = as.numeric(fixed),
    random = as.numeric(random),
    residual = as.numeric(residual),
    marginal = as.numeric(fixed / total),
    conditional = as.numeric((fixed + random) / total)
  )
}

test_that("Johnson (2014) two-level Orange calculations are reproduced", {
  mdl <- fit_johnson_2014_orange_model()
  published_method <- calculate_johnson_2014_components(mdl)
  mimosa <- calculate_r2_components(mdl)

  expect_false(lme4::isSingular(mdl))
  expect_equal(published_method$fixed, 2757.9865, tolerance = 1e-3)
  expect_equal(published_method$random, 389.6174, tolerance = 1e-3)
  expect_equal(published_method$residual, 232.8927, tolerance = 1e-3)
  expect_equal(published_method$marginal, 0.8158525, tolerance = 1e-6)
  expect_equal(published_method$conditional, 0.9311070, tolerance = 1e-6)
  expect_equal(round(published_method$marginal, 2), 0.82)
  expect_equal(round(published_method$conditional, 2), 0.93)
  expect_equal(mimosa, published_method, tolerance = 1e-6)
})

test_that("Johnson (2014) values appear in Mimosa calculation tooltips", {
  mdl <- fit_johnson_2014_orange_model()
  table <- create_table(mdl, l1 = "ageYears", output_options = character(0))

  expect_true(grepl("2757.99 + 389.62 + 232.89", table, fixed = TRUE))
  expect_true(grepl(
    "= 2757.99 / (2757.99 + 389.62 + 232.89) = 0.82",
    table,
    fixed = TRUE
  ))
  expect_true(grepl(
    "= (2757.99 + 389.62) / (2757.99 + 389.62 + 232.89) = 0.93",
    table,
    fixed = TRUE
  ))
  expect_true(grepl(
    "= 389.62 / (389.62 + 232.89) = 0.63",
    table,
    fixed = TRUE
  ))
})
