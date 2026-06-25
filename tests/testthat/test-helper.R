test_that("finding grouping variable works", {
  hsball <- read.csv(test_path("data", "hsball.csv"))
  tutorium <- Hmisc::spss.get(test_path("data", "vl.sav"),
                               use.value.labels = F)
  isabell1 <- Hmisc::spss.get(test_path("data", "MeaTimeAge.sav"), 
                              use.value.labels = F)
  thomas1 <- suppressWarnings(
    Hmisc::spss.get(test_path("data", "Online-Tagebuch_130913.sav"),
                    use.value.labels = F)
  )
  atemm <- Hmisc::spss.get(test_path("data", "ATEMM.sav"), use.value.labels = F)
  karin <- read.csv2(test_path("data", "Joined_data_wellbeing_small.csv"), 
                     fileEncoding = "ISO-8859-1")
  
  isabell2 <- Hmisc::spss.get(test_path("data", "MeaMimosa.sav"), 
                              use.value.labels = F)
  isabell3 <- Hmisc::spss.get(
    test_path("data", "MeaGruppe1Vollstaendig.sav"), 
    use.value.labels = F
  )
  
  kerner1 <- Hmisc::spss.get(test_path("data", "NurEG12-1.sav"), 
                             use.value.labels = F)
  
  # atemm is problematic because too many dummy-vars are introduced
  # such that there is a dependency although there are no further level 2 vars
  
  expect_identical("ID", find_id(hsball))
  expect_identical("ID", find_id(tutorium))
  expect_identical("ID", find_id(isabell1))
  expect_identical("Code", find_id(thomas1))
  expect_identical("ID", find_id(isabell2))
  expect_identical(c("ID", "VPCODE"), find_id(isabell3))
  expect_identical(c("Gruppe", "Frau", "Mann", "Abstinent", "Raucht", 
                     "BildungHoch", "BildungNiedrig"), find_id(atemm))
  # what is this?
  expect_identical(find_id(atemm), find_id(atemm[, 14:1]))
  expect_identical(c("serial"), find_id(karin))
})

test_that("grouping variable explanation is returned", {
  hsball <- read.csv(test_path("data", "hsball.csv"))
  explanation <- explain_find_id(hsball)

  expect_true(is.data.frame(explanation))
  expect_true(all(c(
    "variable",
    "n_groups",
    "repeated_group_prop",
    "repeated_row_prop",
    "median_n",
    "n_variables_lvl2",
    "final_score",
    "is_candidate"
  ) %in% names(explanation)))
  expect_identical("ID", explanation$variable[1])
  expect_true(explanation$is_candidate[1])
  expect_true(explanation$repeated_row_prop[1] > 0)
})

test_that("grouping variable is found in two-level mlmRev datasets", {
  expect_identical("district", find_id(mlmRev::Contraception))
  expect_identical("id", find_id(mlmRev::Early))
  expect_identical("school", find_id(mlmRev::Exam))
  expect_identical("school", find_id(mlmRev::Hsb82))
  expect_identical("region", find_id(mlmRev::Mmmec))
  expect_identical("Subject", find_id(mlmRev::Oxboys))
})

test_that("dependent variables are restricted to numeric variables", {
  data <- data.frame(
    numeric_score = c(1.2, 2.4, 3.1),
    integer_score = c(1L, 2L, 3L),
    group = factor(c("a", "a", "b")),
    condition = c("x", "y", "x")
  )

  expect_identical(
    c("numeric_score", "integer_score"),
    filter_dvs(names(data), data)
  )
  expect_identical(
    "integer_score",
    filter_dvs(c("integer_score", "condition"), data)
  )
})
