library(testthat)
library(shinytest)
library(dplyr)
library(testthat)
library(mlmRev)

test_that("Application works", {
  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
  expect_pass(testApp(".", compareImages = FALSE))
})

# create version without shiny stuff (e.g. progress bars)
script <- readLines("helper.R")
lines <- grepl("progress|withProgress|incProgress", script)
script[lines] <- paste("#", script[lines])
writeLines(script, "helper2.R")
source("helper2.R")

hsball <- read.csv("hsball.csv")
tutorium <- Hmisc::spss.get(file = "Vorlesungsdaten_Mehrebenenanalyse.sav")
isabell1 <- Hmisc::spss.get(file = "MEA - Time & Age.sav", use.value.labels = F)
thomas1 <- Hmisc::spss.get(file = "Online-Tagebuch_130913.sav", use.value.labels = F)
atemm <- Hmisc::spss.get(file = "ATEMM.sav", use.value.labels = F)

karin <- read.csv2("Joined_data_wellbeing.csv", fileEncoding = "ISO-8859-1")
# levels <- determine_levels(c("serial"), karin)
# level2_names <- table(grepl("post|pre|baseline|", levels$level2))["TRUE"]

# test with available data in R and own data
# atemm is problematic because too many dummy-vars are introduced
# such that there is a dependency although there are no further level
# 2 vars
test_that("finding grouping variable works", {
  expect_identical("Subject", find_id(sleepstudy))
  expect_identical("ID", find_id(hsball))
  expect_identical("ID", find_id(tutorium))
  expect_identical("ID", find_id(isabell1))
  expect_identical("Code", find_id(thomas1))
  expect_identical(c("school", "schgend"), find_id(mlmRev::Exam))
  expect_identical(c("Gruppe", "Mann", "Frau", "BildungNiedrig",
                     "BildungHoch", "Abstinent", "Raucht"), find_id(atemm))
  expect_identical(c("serial"), find_id(karin))
  #expect_equal(level2_names, c("TRUE" = 74))
})
