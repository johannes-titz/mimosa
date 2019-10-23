app <- ShinyDriver$new("../")
app$snapshotInit("load_data")

app$uploadFile(datafile = "../Joined_data_wellbeing.csv") # <-- This should be the path to the file, relative to the app's tests/ directory
app$snapshot()
app$uploadFile(datafile = "../Exam.csv") # <-- This should be the path to the file, relative to the app's tests/ directory
app$snapshot()
app$uploadFile(datafile = "../hsball.csv") # <-- This should be the path to the file, relative to the app's tests/ directory
app$snapshot()
app$uploadFile(datafile = "../Daten Time & Age - Tutorium.sav") # <-- This should be the path to the file, relative to the app's tests/ directory
app$snapshot()
app$uploadFile(datafile = "../ATEMM.sav") # <-- This should be the path to the file, relative to the app's tests/ directory