server_model_inputs <- function(session, example, group_id, dv, l1 = character(0),
                                l1_varies = character(0), l2 = character(0),
                                interaction = character(0), reactive_mode = TRUE,
                                start_calculation_button = 0) {
  session$setInputs(examplefile = example)
  session$flushReact()
  session$setInputs(
    reactive_mode = reactive_mode,
    start_calculation_button = start_calculation_button,
    group_id = group_id,
    dv = dv,
    l1 = l1,
    l1_varies = l1_varies,
    l2 = l2,
    interaction = interaction,
    output_options = character(0),
    nAGQ = 1
  )
  session$flushReact()
}

test_that("server fits the Gaussian Orange example and generates copyable code", {
  shiny::testServer(server, {
    server_model_inputs(
      session,
      example = "johnson2014-orange",
      group_id = "Tree",
      dv = "circumference",
      l1 = "ageYears"
    )

    expect_equal(nrow(reactive$data), 35)
    expect_identical(reactive$group_id_selected, "Tree")
    expect_identical(output$mod_r, "circumference ~ ageYears + (1 | Tree)")
    expect_true(grepl("lme4::lmer", output$r_analysis_code, fixed = TRUE))
    expect_true(grepl("sjPlot::tab_model(model)", output$r_analysis_code,
                      fixed = TRUE))

    table <- paste(as.character(output$table_region), collapse = "")
    expect_true(grepl("Model summary", table, fixed = TRUE))
    expect_true(grepl("2757.99", table, fixed = TRUE))
    expect_true(grepl("0.82", table, fixed = TRUE))

    variables <- paste(as.character(output$variables), collapse = "")
    expect_true(grepl("Dependent variable", variables, fixed = TRUE))
    expect_true(grepl("circumference", variables, fixed = TRUE))
    expect_true(grepl("Tree", variables, fixed = TRUE))
    expect_true(grepl(
      "circumference",
      paste(as.character(output$mod_l1), collapse = ""),
      fixed = TRUE
    ))
    expect_true(grepl(
      "beta",
      paste(as.character(output$mod_l2), collapse = ""),
      ignore.case = TRUE
    ))
  })
})

test_that("server automatically fits binomial examples with the documented event", {
  shiny::testServer(server, {
    server_model_inputs(
      session,
      example = "mlmRev::Contraception",
      group_id = "district",
      dv = "use",
      l1 = "age"
    )

    expect_identical(output$mod_r, "use ~ age + (1 | district)")
    expect_true(grepl("lme4::glmer", output$r_analysis_code, fixed = TRUE))
    expect_true(grepl('# Model "Y" as the event (1)',
                      output$r_analysis_code, fixed = TRUE))
    expect_true(grepl("family = stats::binomial()",
                      output$r_analysis_code, fixed = TRUE))
    expect_true(grepl(
      "Model summary",
      paste(as.character(output$table_region), collapse = ""),
      fixed = TRUE
    ))
  })
})

test_that("manual server mode waits for the estimate button", {
  shiny::testServer(server, {
    server_model_inputs(
      session,
      example = "johnson2014-orange",
      group_id = "Tree",
      dv = "circumference",
      l1 = "ageYears",
      reactive_mode = FALSE,
      start_calculation_button = 0
    )

    expect_null(output$table_region)
    expect_identical(output$mod_r, "")

    session$setInputs(start_calculation_button = 1)
    session$flushReact()

    expect_identical(output$mod_r, "circumference ~ ageYears + (1 | Tree)")
    expect_true(grepl(
      "0.82",
      paste(as.character(output$table_region), collapse = ""),
      fixed = TRUE
    ))
  })
})

test_that("server loads uploaded CSV data without browser automation", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  uploaded <- data.frame(
    ID = rep(seq_len(10), each = 3),
    outcome = seq_len(30),
    predictor = rep(0:2, 10)
  )
  utils::write.csv(uploaded, path, row.names = FALSE)

  shiny::testServer(server, {
    session$setInputs(datafile = list(
      name = "uploaded.csv",
      datapath = path,
      size = file.info(path)$size,
      type = "text/csv"
    ))
    session$flushReact()

    expect_equal(reactive$data, uploaded)
    expect_identical(reactive$group_id_selected, "ID")
    expect_true(grepl('utils::read.csv(', reactive$data_source_code,
                      fixed = TRUE))
    expect_true(grepl("Dependent variable",
                      paste(as.character(output$variables), collapse = ""),
                      fixed = TRUE))
  })
})
