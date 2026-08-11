test_that("finding grouping variable works", {
  hsball <- read.csv(test_path("data", "hsball.csv"))
  tutorium <- read_sav(test_path("data", "vl.sav"))
  isabell1 <- read_sav(test_path("data", "MeaTimeAge.sav"))
  thomas1 <- suppressWarnings(
    read_sav(test_path("data", "Online-Tagebuch_130913.sav"))
  )
  atemm <- read_sav(test_path("data", "ATEMM.sav"))
  karin <- read.csv2(test_path("data", "Joined_data_wellbeing_small.csv"), 
                     fileEncoding = "ISO-8859-1")
  
  isabell2 <- read_sav(test_path("data", "MeaMimosa.sav"))
  isabell3 <- read_sav(test_path("data", "MeaGruppe1Vollstaendig.sav"))
  
  kerner1 <- read_sav(test_path("data", "NurEG12-1.sav"))
  
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

test_that("grouping detection handles data without a plausible ID", {
  data <- data.frame(unique_value = seq_len(5), constant = 1)
  explanation <- explain_find_id(data)

  expect_s3_class(explanation, "data.frame")
  expect_equal(nrow(explanation), 0)
  expect_identical(names(explanation), names(empty_id_explanation()))
  expect_identical(find_id(data), names(data))

  all_missing <- score_group_candidate(
    data.frame(candidate = rep(NA_integer_, 4)),
    "candidate"
  )
  expect_equal(all_missing$repeated_row_prop, 0)
  expect_equal(all_missing$n_groups, 0)
  expect_equal(score_group_count(8, 10), 0.4)
})

test_that("grouping variable is found in two-level mlmRev datasets", {
  expect_identical("school", find_id(mlmRev::Chem97))
  expect_identical("district", find_id(mlmRev::Contraception))
  expect_identical("id", find_id(mlmRev::Early))
  expect_identical("school", find_id(mlmRev::Exam))
  expect_identical("school", find_id(mlmRev::Hsb82))
  expect_identical("region", find_id(mlmRev::Mmmec))
  expect_identical("Subject", find_id(mlmRev::Oxboys))
})

test_that("dependent variables include numeric and dichotomous variables", {
  data <- data.frame(
    numeric_score = c(1.2, 2.4, 3.1),
    integer_score = c(1L, 2L, 3L),
    group = factor(c("a", "a", "b")),
    condition = c("x", "y", "x"),
    three_groups = c("x", "y", "z")
  )

  expect_identical(
    c("numeric_score", "integer_score", "group", "condition"),
    filter_dvs(names(data), data)
  )
  expect_identical(
    c("integer_score", "condition"),
    filter_dvs(c("integer_score", "condition", "three_groups"), data)
  )
})

test_that("example data set choices include two-level mlmRev data", {
  choices <- unname(example_dataset_choices())
  expected <- c(
    "mlmRev::Exam",
    "mlmRev::Chem97",
    "mlmRev::Contraception",
    "mlmRev::Early",
    "mlmRev::Hsb82",
    "mlmRev::Mmmec",
    "mlmRev::Oxboys"
  )

  expect_true(all(expected %in% choices))
  expect_s3_class(load_example_dataset("mlmRev::Chem97"), "data.frame")
  expect_s3_class(load_example_dataset("mlmRev::Contraception"), "data.frame")
  expect_s3_class(load_example_dataset("mlmRev::Oxboys"), "data.frame")
  expect_true("johnson2014-orange" %in% choices)
  expect_s3_class(load_example_dataset("johnson2014-orange"), "data.frame")
  expect_s3_class(load_example_dataset("mimosa::popular2"), "data.frame")
  expect_true(nzchar(example_dataset_description("mlmRev::Exam")))
  expect_true(grepl("students nested in schools", example_dataset_description("mlmRev::Chem97"), fixed = TRUE))
})

test_that("example data set helpers tolerate empty input", {
  expect_null(load_example_dataset(NULL))
  expect_null(load_example_dataset(character(0)))
  expect_null(load_example_dataset(c("mlmRev::Exam", "mlmRev::Chem97")))
  expect_identical("", example_dataset_description(NULL))
})

test_that("all example loaders and selector elements are usable", {
  keys <- unname(example_dataset_choices())
  loaded <- lapply(keys, load_example_dataset)

  expect_true(all(vapply(loaded, is.data.frame, logical(1))))
  expect_identical(example_dataset_code(NULL), "")
  expect_identical(example_dataset_code("not-an-example"), "")
  expect_true(grepl("data(\"popular2\"", example_dataset_code("popular2"),
                    fixed = TRUE))

  options <- example_dataset_options("popular2")
  options_html <- paste(vapply(options, as.character, character(1)),
                        collapse = "")
  select_html <- paste(as.character(example_dataset_select("popular2")),
                       collapse = "")
  expect_true(grepl('value="popular2"', options_html, fixed = TRUE))
  expect_true(grepl('selected="selected"', options_html, fixed = TRUE))
  expect_true(grepl('id="examplefile"', select_html, fixed = TRUE))
})

test_that("Exam example defaults select the tutorial model", {
  result <- determine_levels("school", mlmRev::Exam)
  level1 <- filter_ivs(result$level1, mlmRev::Exam)
  dv_choices <- filter_dvs(level1, mlmRev::Exam)

  expect_identical(
    "normexam",
    default_exam_value("mlmRev::Exam", "normexam", dv_choices)
  )
  expect_identical(
    c("standLRT", "sex"),
    default_exam_values("mlmRev::Exam", c("standLRT", "sex"), level1)
  )
  expect_identical(
    "standLRT",
    default_exam_values("mlmRev::Exam", "standLRT", c("standLRT", "sex"))
  )
  expect_identical(
    character(0),
    default_exam_values("lme4::sleepstudy", c("standLRT", "sex"), level1)
  )
})
