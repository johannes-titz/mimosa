test_that("load_data() handles all input types", {
  # Create sample data
  csv2 <- test_path("data", "Joined_data_wellbeing_small.csv")
  csv2_df <- read.csv2(csv2)
  csv <- test_path("data", "Exam.csv")
  csv_df <- read.csv(csv)
  sav <- test_path("data", "ATEMM.sav")
  sav_df <- read_sav(sav)
  
  expect_equal(load_data("Exam.csv", csv), csv_df)
  expect_equal(load_data("Joined_data_wellbeing_small.csv", csv2), csv2_df)
  expect_equal(load_data("ATEMM.sav", sav), sav_df)
  expect_error(load_data("blah.rnd", csv), "Invalid file")
})

test_that("read_sav preserves Mimosa's SPSS import conventions", {
  data <- read_sav(test_path("data", "ATEMM.sav"))

  expect_s3_class(data, "data.frame")
  expect_identical(dim(data), c(524L, 14L))
  expect_true(all(c("SER.MW.alle", "PHQ.Score.Depr.t0") %in% names(data)))
  expect_identical(typeof(data$ID), "integer")
  expect_identical(class(data$ID), c("labelled", "numeric"))
  expect_identical(
    unname(attr(data$ID, "label")),
    "Patient durchnummeriert Arm B"
  )
})
test_that("uploaded data code and readers handle unsupported and fallback cases", {
  expect_identical(uploaded_dataset_code("data.unsupported"), "")

  attempts <- 0
  fallback_reader <- function(path, fileEncoding = NULL) {
    attempts <<- attempts + 1
    if (is.null(fileEncoding)) {
      stop("try an explicit encoding")
    }
    data.frame(value = 1)
  }
  result <- read_csv_with_fallback("unused.csv", fallback_reader)
  expect_s3_class(result, "data.frame")
  expect_equal(attempts, 2)

  failing_reader <- function(path, fileEncoding = NULL) stop("unreadable")
  expect_error(
    read_csv_with_fallback("unused.csv", failing_reader),
    "unreadable"
  )
})
