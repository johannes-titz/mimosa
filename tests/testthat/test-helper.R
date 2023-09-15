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
  karin <- read.csv2(test_path("data", "Joined_data_wellbeing.csv"), 
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
  expect_identical(c("ID", "Min.Unpuenkt", "Pktl.02", "Pktl.02i", "Pktl.01", 
                     "Pktl.01i", "SF"), find_id(isabell2))
  expect_identical(c("Gruppe", "Frau", "Mann", "Abstinent", "Raucht", 
                     "BildungHoch", "BildungNiedrig"), find_id(atemm))
  # what is this?
  expect_identical(find_id(atemm), find_id(atemm[, 14:1]))
  expect_identical(c("serial"), find_id(karin))
})
