test_that("UI and Shiny app objects are constructed without launching a browser", {
  old_options <- options(shiny.testmode = TRUE)
  on.exit(options(old_options), add = TRUE)

  ui <- myui()
  app <- run_app(host = "127.0.0.1", port = 12345)

  expect_s3_class(ui, "shiny.tag")
  expect_true(grepl("mimosa v", as.character(ui), fixed = TRUE))
  expect_s3_class(app, "shiny.appobj")
  expect_identical(app$options$host, "127.0.0.1")
  expect_identical(app$options$port, 12345)
})
