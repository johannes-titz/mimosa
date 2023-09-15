library(shinytest2)

# https://multilevel-analysis.sites.uu.nl/wp-content/uploads/sites/27/2018/07/Mplus-tutorial.pdf
test_that("Hox Example Popularity is reproducible", {
  # to modify shinyjs behavior so that R CMD check also runs
  options(shiny.testmode = T)
  app <- AppDriver$new(run_app())
  
  app$upload_file(datafile = test_path("data", "popular2.csv"))
  
  app$set_inputs(dv = "popular")
  outputtable <- app$get_value(output = "table_region")$html
  # always in column 2
  # intercept
  expect_true(grepl('2">5.08', outputtable))
  # sigma^2
  expect_true(grepl('2">1.22', outputtable))
  # tau
  expect_true(grepl('2">0.70', outputtable))
  # ICC
  expect_true(grepl('2">0.36', outputtable))
})