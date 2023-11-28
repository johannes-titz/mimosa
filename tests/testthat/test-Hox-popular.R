library(shinytest2)

# https://multilevel-analysis.sites.uu.nl/wp-content/uploads/sites/27/2018/07/Mplus-tutorial.pdf
test_that("Hox Example Popularity is reproducible", {
  # to modify shinyjs behavior so that R CMD check also runs
  skip_on_ci()
  options(shiny.testmode = F)
  app <- AppDriver$new(run_app())
  
  app$upload_file(datafile = test_path("data", "popular2.sav"))
  
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
  
  # last model, with level 2 effect and cross-level-interaction
  # note that sex is not a random effect
  app$set_inputs(l1 = c("extrav", "sex"),  l2 = c("texp"))
  app$set_inputs(l1_varies = c("extrav"))
  app$set_inputs(interaction = c("extrav:texp"))

  outputtable <- app$get_value(output = "table_region")$html
  # always in column 2
  # a teacher with 0 years of experience has an expected popularity of -1.207
  # intercept &#45; = -
  expect_true(grepl('2">&#45;1.21', outputtable))
  # extraversion effect
  expect_true(grepl('2">0.80', outputtable))
  # teacherxperience effect
  expect_true(grepl('2">0.23', outputtable))
  # moderator of teacherxp on relationship between extraversion and popularity
  expect_true(grepl('2">&#45;0.02', outputtable))
  
  # sigma^2
  expect_true(grepl('2">0.55', outputtable))
  # tau seems to differ slightly
  # ICC not clear
  # se and t-values also differ slightly, not the best test case, but even in
  # the manual they say that thes values are different from the ones in the book
})