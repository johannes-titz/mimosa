test_that("load_data() handles all input types", {
  # Create sample data
  csv2 <- test_path("data", "Joined_data_wellbeing_small.csv")
  csv2_df <- read.csv2(csv2)
  csv <- test_path("data", "Exam.csv")
  csv_df <- read.csv(csv)
  sav <- test_path("data", "ATEMM.sav")
  sav_df <- Hmisc::spss.get(sav, use.value.labels = F)
  
  expect_equal(load_data("Exam.csv", csv), csv_df)
  expect_equal(load_data("Joined_data_wellbeing_small.csv", csv2), csv2_df)
  expect_equal(load_data("ATEMM.sav", sav), sav_df)
  expect_error(load_data("blah.rnd", csv), "Invalid file")
})