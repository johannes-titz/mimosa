test_that("model panel exposes complete code and copy controls", {
  html <- as.character(ui_body(testing = TRUE))

  expect_true(grepl('id="mod_r"', html, fixed = TRUE))
  expect_true(grepl("R analysis code", html, fixed = TRUE))
  expect_true(grepl('id="copy_r_code"', html, fixed = TRUE))
  expect_true(grepl('id="r_analysis_code"', html, fixed = TRUE))
  expect_true(grepl("mimosaCopyRCode", html, fixed = TRUE))
  expect_true(grepl("mimosaCopyCitation", html, fixed = TRUE))
  expect_true(grepl(
    'class="shiny-html-output shiny-table-output" id="table_region"',
    html,
    fixed = TRUE
  ))
  expect_lt(
    regexpr('id="r_analysis_code"', html, fixed = TRUE)[1],
    regexpr('id="copy_r_code"', html, fixed = TRUE)[1]
  )
})

test_that("R formulas match the selected fixed and random effects", {
  expect_identical(
    create_r_formula(
      "score", "school", l1 = c("age", "sex"), l2 = "sector",
      l1_varies = "age", interaction = "age:sector"
    ),
    "score ~ age + sex + sector + age:sector + (1 + age | school)"
  )
  expect_identical(
    create_r_formula("score", "school"),
    "score ~ (1 | school)"
  )
  expect_identical(
    create_r_formula("test score", "school id", l1 = "years taught"),
    "`test score` ~ `years taught` + (1 | `school id`)"
  )
})

test_that("display equations cover fixed, random, and moderated slopes", {
  level1 <- create_equation("score", c("time", "condition"))
  expect_true(grepl("score<sub>ij</sub>", level1, fixed = TRUE))
  expect_true(grepl("time<sub>ij</sub>", level1, fixed = TRUE))
  expect_true(grepl("condition<sub>ij</sub>", level1, fixed = TRUE))
  expect_true(grepl("e<sub>ij</sub>", level1, fixed = TRUE))

  intercept <- create_lvl2_constant(c("sector", "size"))
  expect_true(grepl("sector<sub>j</sub>", intercept, fixed = TRUE))
  expect_true(grepl("size<sub>j</sub>", intercept, fixed = TRUE))
  expect_true(grepl("u<sub>0j</sub>", intercept, fixed = TRUE))

  slope <- paste(create_mdl2_formula(
    beta_nmbr = 1,
    beta_varies = TRUE,
    interaction = c("time:sector", "time:size")
  ), collapse = "")
  expect_true(grepl("u<sub>1j</sub>", slope, fixed = TRUE))
  expect_true(grepl("time:sector", slope, fixed = TRUE))
  expect_true(grepl("time:size", slope, fixed = TRUE))

  expect_identical(
    who_moderates_me("time", c("time:sector", "time:size")),
    c("sector", "size")
  )
  expect_null(who_moderates_me("condition", "time:sector"))
  expect_null(who_moderates_me("time", character(0)))
})

test_that("complete Gaussian analysis code is valid and reproducible", {
  formula <- create_r_formula(
    "circumference", "Tree", l1 = "ageYears"
  )
  code <- create_analysis_code(
    formula,
    example_dataset_code("johnson2014-orange"),
    dv = "circumference",
    family = "gaussian"
  )

  expect_no_error(parse(text = code))
  compact_code <- gsub("[[:space:]]+", " ", code)
  expect_true(grepl("data <- transform( datasets::Orange", compact_code, fixed = TRUE))
  expect_true(grepl("model <- lme4::lmer(", code, fixed = TRUE))
  expect_true(grepl(
    "formula = circumference ~ ageYears + (1 | Tree)",
    compact_code,
    fixed = TRUE
  ))
  expect_true(grepl("sjPlot::tab_model(model)", code, fixed = TRUE))
  expect_false(grepl("summary(model)", code, fixed = TRUE))
  expect_false(grepl("insight::get_variance", code, fixed = TRUE))
  expect_lte(max(nchar(strsplit(code, "\n", fixed = TRUE)[[1]])), 50)
})

test_that("complete binomial analysis code preserves Mimosa event coding", {
  code <- create_analysis_code(
    "outcome ~ predictor + (1 | group)",
    "data <- example_data",
    dv = "outcome",
    family = "binomial",
    event = "yes",
    nAGQ = 3
  )

  expect_no_error(parse(text = code))
  compact_code <- gsub("[[:space:]]+", " ", code)
  expect_true(grepl('# Model "yes" as the event (1)', code, fixed = TRUE))
  expect_true(grepl('data[["outcome"]] == "yes"', compact_code, fixed = TRUE))
  expect_true(grepl("model <- lme4::glmer(", code, fixed = TRUE))
  expect_true(grepl("family = stats::binomial()", code, fixed = TRUE))
  expect_true(grepl("nAGQ = 3", code, fixed = TRUE))
  expect_lte(max(nchar(strsplit(code, "\n", fixed = TRUE)[[1]])), 50)
})

test_that("long formulas wrap safely around backticked names and operators", {
  formula <- create_r_formula(
    "a response with spaces",
    "a grouping variable with spaces",
    l1 = c("a long predictor name", "another long predictor name"),
    l1_varies = c("a long predictor name", "another long predictor name")
  )
  code <- create_analysis_code(
    formula,
    "data <- example_data",
    dv = "a response with spaces",
    family = "gaussian"
  )

  expect_no_error(parse(text = code))
  expect_gt(length(strsplit(code, "\n", fixed = TRUE)[[1]]), 10)
  expect_true(grepl("`a long predictor name`", code, fixed = TRUE))
})

test_that("data loading code covers examples and uploaded files", {
  expect_identical(
    example_dataset_code("lme4::sleepstudy"),
    "data <- lme4::sleepstudy"
  )
  expect_true(grepl("read.csv", uploaded_dataset_code("results.csv")))
  expect_true(grepl("read.csv2", uploaded_dataset_code("results.csv")))
  expect_no_error(parse(text = uploaded_dataset_code("results.csv")))
  expect_no_error(parse(text = uploaded_dataset_code("results.sav")))
  expect_true(grepl("mimosa::read_sav", uploaded_dataset_code("results.sav")))
  expect_identical(uploaded_dataset_code("results.txt"), "")
})
