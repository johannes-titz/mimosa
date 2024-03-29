# DO NOT FORGET TO BUILD THE APP FIRST because testing works through
# loading the mimosa package via library, if you do not reinstall
# mimosa, then the test will be run with the old package version
# library(testthat)
library(shinytest)
# library(dplyr)
library(testthat)
library(dplyr)
# library(mlmRev)

test_that("Application works", {
  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
  expect_pass(testApp("tests/testthat/app/", compareImages = FALSE))
})

# these are additional tests, but they require datasets for which I do not
# have permissions of any kind, so I will just run them locally

# create version without shiny stuff (e.g. progress bars)
script <- readLines("R/helper.R")
lines <- grepl("progress|withProgress|incProgress", script)
script[lines] <- paste("#", script[lines])
writeLines(script, "helper2.R")
source("helper2.R")

hsball <- read.csv("tests/testthat/app/tests/hsball.csv")
tutorium <- Hmisc::spss.get(file = "Vorlesungsdaten_Mehrebenenanalyse.sav")
isabell1 <- Hmisc::spss.get(file = "MEA - Time & Age.sav", use.value.labels = F)
thomas1 <- Hmisc::spss.get(file = "Online-Tagebuch_130913.sav", use.value.labels = F)
atemm <- Hmisc::spss.get(file = "tests/testthat/app/tests/ATEMM.sav", use.value.labels = F)

karin <- read.csv2("Joined_data_wellbeing.csv", fileEncoding = "ISO-8859-1")
# levels <- determine_levels(c("serial"), karin)
# level2_names <- table(grepl("post|pre|baseline|", levels$level2))["TRUE"]

isabell2 <- Hmisc::spss.get(file = "mimosa_isabell/MEA Daten - Mimosa.sav", use.value.labels = F)
isabell3 <- Hmisc::spss.get(file = "mimosa_isabell/MEA Daten - Gruppe 1 - vollständig.sav", use.value.labels = F)

kerner1 <- Hmisc::spss.get(file = "Nur EG 12.1.sav", use.value.labels = F)

# test with available data in R and own data
# atemm is problematic because too many dummy-vars are introduced
# such that there is a dependency although there are no further level
# 2 vars
test_that("finding grouping variable works", {
  expect_identical("ID", find_id(hsball))
  expect_identical("ID", find_id(tutorium))
  expect_identical("ID", find_id(isabell1))
  expect_identical("Code", find_id(thomas1))
  expect_identical(c("ID", "Min.Unpuenkt", "Pktl.02", "Pktl.02i", "Pktl.01", "Pktl.01i", "SF"), find_id(isabell2))
  expect_identical(c("Gruppe", "Frau", "Mann", "Abstinent", "Raucht", 
                     "BildungHoch", "BildungNiedrig"), find_id(atemm))
  expect_identical(find_id(atemm), find_id(atemm[,14:1]))
  expect_identical(c("serial"), find_id(karin))
  #expect_equal(level2_names, c("TRUE" = 74))
})

file.remove("helper2.R")