library(shinytest2)

expect_html_value <- function(html, value) {
  expect_true(grepl(paste0(">", value, "(<|&nbsp;)"), html))
}

# https://multilevel-analysis.sites.uu.nl/wp-content/uploads/sites/27/2018/07/Mplus-tutorial.pdf
test_that("Hox Example Popularity is reproducible", {
  # to modify shinyjs behavior so that R CMD check also runs
  skip_on_ci()
  options(shiny.testmode = F)
  app <- AppDriver$new(run_app())
  
  app$upload_file(datafile = test_path("data", "popular2.sav"))
  
  app$set_inputs(dv = "popular")
  outputtable <- app$get_value(output = "table_region")$html
  # intercept
  expect_html_value(outputtable, "5.08")
  # sigma^2
  expect_html_value(outputtable, "1.22")
  # tau
  expect_html_value(outputtable, "0.70")
  # ICC
  expect_html_value(outputtable, "0.36")
  
  # last model, with level 2 effect and cross-level-interaction
  # note that sex is not a random effect
  app$set_inputs(l1 = c("extrav", "sex"),  l2 = c("texp"))
  app$set_inputs(l1_varies = c("extrav"))
  app$set_inputs(interaction = c("extrav:texp"))

  outputtable <- app$get_value(output = "table_region")$html
  # a teacher with 0 years of experience has an expected popularity of -1.207
  # intercept &#45; = -
  expect_html_value(outputtable, "&#45;1.21")
  # extraversion effect
  expect_html_value(outputtable, "0.80")
  # teacherxperience effect
  expect_html_value(outputtable, "0.23")
  # moderator of teacherxp on relationship between extraversion and popularity
  expect_html_value(outputtable, "&#45;0.02")
  
  # sigma^2
  expect_html_value(outputtable, "0.55")
  # tau seems to differ slightly
  # ICC not clear
  # se and t-values also differ slightly, not the best test case, but even in
  # the manual they say that thes values are different from the ones in the book
})
