library(shinytest2)

test_that("Hox Example Popularity is reproducible", {
  app <- AppDriver$new(run_app())
  
  app$upload_file(datafile = "data/popular2.sav")
  app$set_inputs(dv = "popular")
  outputtable <- app$get_value(output = "table_region")$html
  # intercept
  expect_true(grepl('col2">5.08', outputtable))
  # sigma^2
  expect_true(grepl('"2">1.22', outputtable))
  # tau
  expect_true(grepl('2">0.70', outputtable))
  # ICC
  expect_true(grepl('2">0.36', outputtable))
})