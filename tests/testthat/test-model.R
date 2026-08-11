test_that("response family is selected automatically", {
  expect_true(is_dichotomous_response(c("no", "yes", NA)))
  expect_false(is_dichotomous_response(c("a", "b", "c")))
  expect_identical(response_family(c(0, 1, 0, NA)), "binomial")
  expect_identical(response_family(c(1, 2, 1, 2)), "binomial")
  expect_identical(response_family(factor(c("no", "yes", "no"))), "binomial")
  expect_identical(response_family(c(TRUE, FALSE, TRUE)), "binomial")
  expect_identical(response_family(c(1.2, 2.4, 3.1)), "gaussian")
  expect_true(is.na(response_family(c("a", "b", "c"))))
})

test_that("unsupported responses fail before model estimation", {
  data <- data.frame(outcome = c("a", "b", "c"))

  expect_error(
    prepare_mixed_model_data(data, "outcome"),
    "numeric or dichotomous"
  )
  expect_error(
    fit_mixed_model(outcome ~ 1, data, "outcome"),
    "numeric or dichotomous"
  )
})

test_that("dichotomous responses are recoded with a documented event", {
  numeric_data <- data.frame(outcome = c(1, 2, 1, NA, 2))
  numeric_model_data <- prepare_mixed_model_data(numeric_data, "outcome")

  expect_identical(numeric_model_data$family, "binomial")
  expect_equal(numeric_model_data$event, 2)
  expect_equal(numeric_model_data$data$outcome, c(0, 1, 0, NA, 1))

  factor_data <- data.frame(
    outcome = factor(c("yes", "no", "yes"), levels = c("yes", "no"))
  )
  factor_model_data <- prepare_mixed_model_data(factor_data, "outcome")

  expect_identical(factor_model_data$event, "no")
  expect_equal(factor_model_data$data$outcome, c(0, 1, 0))
})

test_that("automatic model fitting uses glmer for a dichotomous response", {
  set.seed(42)
  group <- factor(rep(seq_len(30), each = 10))
  predictor <- stats::rnorm(length(group))
  group_effect <- stats::rnorm(nlevels(group), 0, 0.7)
  probability <- stats::plogis(-0.3 + 0.8 * predictor + group_effect[group])
  outcome <- ifelse(stats::rbinom(length(group), 1, probability) == 1, 2, 1)
  data <- data.frame(outcome, predictor, group)

  mdl <- fit_mixed_model(
    outcome ~ predictor + (1 | group),
    data,
    dv = "outcome"
  )

  expect_s4_class(mdl, "glmerMod")
  expect_identical(attr(mdl, "mimosa_response_family"), "binomial")
  expect_equal(attr(mdl, "mimosa_event"), 2)
  expect_equal(sort(unique(stats::model.response(stats::model.frame(mdl)))), 0:1)

  data$outcome <- factor(
    ifelse(data$outcome == 2, "yes", "no"),
    levels = c("no", "yes")
  )
  factor_mdl <- fit_mixed_model(
    outcome ~ predictor + (1 | group),
    data,
    dv = "outcome"
  )

  expect_s4_class(factor_mdl, "glmerMod")
  expect_identical(attr(factor_mdl, "mimosa_event"), "yes")
  expect_equal(
    sort(unique(stats::model.response(stats::model.frame(factor_mdl)))),
    0:1
  )
})

test_that("automatic model fitting keeps continuous responses Gaussian", {
  mdl <- fit_mixed_model(
    Reaction ~ Days + (1 | Subject),
    lme4::sleepstudy,
    dv = "Reaction"
  )

  expect_s4_class(mdl, "lmerMod")
  expect_identical(attr(mdl, "mimosa_response_family"), "gaussian")
  expect_null(attr(mdl, "mimosa_event"))
})
