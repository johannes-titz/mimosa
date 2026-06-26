test_that("fixed-effect variance is calculated empirically", {
  mdl <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  expected <- stats::var(as.vector(stats::model.matrix(mdl) %*% lme4::fixef(mdl)))

  expect_equal(calculate_fixed_effect_variance(mdl), expected)
  expect_gt(calculate_fixed_effect_variance(mdl), 0)
})

test_that("fixed-effect variance is shown in model table", {
  mdl <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  table <- create_table(mdl, l1 = "Days", output_options = character(0))
  fixed_var <- format_table_number(calculate_fixed_effect_variance(mdl))

  expect_false(grepl("Var\\(dependent variable\\)", table))
  expect_true(grepl("&sigma;<sup>2</sup><sub>FE</sub>", table, fixed = TRUE))
  expect_true(grepl(fixed_var, table, fixed = TRUE))
  expect_false(grepl("\\\\(", table, fixed = TRUE))
})

test_that("variance summary rows include labels and tooltips", {
  mdl <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  table <- create_table(mdl, l1 = "Days", output_options = character(0))

  expect_true(grepl("&sigma;<sup>2</sup>", table, fixed = TRUE))
  expect_true(grepl("Estimated residual variance", table, fixed = TRUE))
  expect_true(grepl("Estimated random-effect variance", table, fixed = TRUE))
  expect_true(grepl("Marginal R-squared: fixed-effects share", table, fixed = TRUE))
  expect_true(grepl("Conditional R-squared: fixed plus random effects share", table, fixed = TRUE))
  expect_true(grepl("R<sup>2</sup><sub>m</sub>", table, fixed = TRUE))
  expect_true(grepl("R<sup>2</sup><sub>c</sub>", table, fixed = TRUE))
  expect_true(grepl("908.95 + 1698.08 + 654.94", table, fixed = TRUE))
})

test_that("tau values are moved to predictor table", {
  mdl <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  table <- create_table(mdl, l1 = "Days", output_options = character(0))

  expect_true(grepl("Model summary", table, fixed = TRUE))
  expect_true(grepl("class=\"depvarhead firsttablerow col4\"", table, fixed = TRUE))
  expect_true(grepl("&tau; for (Intercept)", table, fixed = TRUE))
  expect_true(grepl("&tau; for Days", table, fixed = TRUE))
  expect_false(grepl("&tau;<sub>00</sub>", table, fixed = TRUE))
  expect_false(grepl("&tau;<sub>11</sub>", table, fixed = TRUE))
  expect_true(grepl("<td colspan=\"5\" class=\"randomparts\">Model summary</td>", table, fixed = TRUE))
})

test_that("rho values are moved to predictor table", {
  mdl <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  table <- create_table(mdl, l1 = "Days", output_options = character(0))

  expect_true(grepl("class=\"depvarhead firsttablerow col5\">&rho;", table, fixed = TRUE))
  expect_true(grepl("&rho; for Days", table, fixed = TRUE))
  expect_true(grepl("random intercept and this predictor's random slope", table, fixed = TRUE))
  expect_false(grepl("&rho;<sub>01</sub>", table, fixed = TRUE))
})

test_that("ICC summary row includes tooltip", {
  mdl <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  table <- create_table(mdl, l1 = "Days", output_options = character(0))

  expect_true(grepl("Intraclass correlation coefficient", table, fixed = TRUE))
  expect_true(grepl("1698.08 / (1698.08 + 654.94) = 0.72", table, fixed = TRUE))
})

test_that("tooltips are shown on values rather than labels", {
  mdl <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  table <- create_table(mdl, l1 = "Days", output_options = character(0))

  expect_true(grepl("<td class=\"tdata leftalign summary\">&sigma;<sup>2</sup></td>", table, fixed = TRUE))
  expect_true(grepl("<td class=\"tdata leftalign summary\">ICC</td>", table, fixed = TRUE))
  expect_true(grepl("<td class=\"tdata leftalign summary\">Marginal R<sup>2</sup> / Conditional R<sup>2</sup></td>", table, fixed = TRUE))
})

test_that("multiple random slopes get predictor-specific rho values", {
  mdl <- lme4::lmer(
    score ~ age + gcsescore + (age + gcsescore | school),
    data = mlmRev::Chem97,
    REML = FALSE
  )
  table <- create_table(mdl, l1 = c("age", "gcsescore"), output_options = character(0))

  expect_true(grepl("&rho; for age", table, fixed = TRUE))
  expect_true(grepl("&rho; for gcsescore", table, fixed = TRUE))
  expect_true(grepl("group-level random intercept and this predictor's random slope", table, fixed = TRUE))
  expect_false(grepl("&rho;<sub>01</sub>", table, fixed = TRUE))
})

test_that("intercept-only model has zero fixed-effect variance", {
  mdl <- lme4::lmer(Reaction ~ 1 + (1 | Subject), lme4::sleepstudy)

  expect_equal(calculate_fixed_effect_variance(mdl), 0)
})
