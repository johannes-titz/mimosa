context("app-function")
# This file is for testing the applications in the apps/ directory.

library(shinytest)

test_that("mimosa works", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
   expect_pass(testApp("app/", c("mytest", "table_options", "formula",
                                 "mlmRev"),
                       compareImages = FALSE))
})

test_that("finding grouping variable works", {
  # first variable is always the correct id
  expect_identical("schoolNR", find_id(mlmRev::bdf))
  expect_identical(c("school", "schgend"), find_id(mlmRev::Exam))
  expect_identical(find_id(mlmRev::Exam), find_id(mlmRev::Exam[10:1]))
  expect_identical("Subject", find_id(lme4::sleepstudy))
  expect_identical(c("district", "livch", "use", "urban"), find_id(mlmRev::Contraception))
  expect_identical(find_id(mlmRev::Contraception), find_id(mlmRev::Contraception[,6:1]))
  expect_identical("id", find_id(mlmRev::Early))
  expect_identical(c("school", "gender"), find_id(mlmRev::Gcsemv))
  expect_identical(find_id(mlmRev::Gcsemv), find_id(mlmRev::Gcsemv[5:1]))
  expect_identical("school", find_id(mlmRev::Hsb82))
  expect_identical(c("Subject", "Occasion"), find_id(mlmRev::Oxboys))
  expect_identical(find_id(mlmRev::Oxboys), find_id(mlmRev::Oxboys[4:1]))
})
