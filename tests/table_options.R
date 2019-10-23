app <- ShinyDriver$new("../")
app$snapshotInit("table_options")

app$uploadFile(datafile = "../Exam.csv") # <-- This should be the path to the file, relative to the app's tests/ directory
app$setInputs(dv = "normexam")
app$setInputs(l1 = "standLRT")
app$setInputs(l1_varies = "standLRT")
app$setInputs(l2 = "type")
app$setInputs(interaction = "")
app$setInputs(interaction = "standLRT:type")
app$setInputs(output_options = c("standard error", "AIC", "Deviance", "Log-Likelihood", "standardized coefficients", "test statistic", "p-value"))
Sys.sleep(0.5)
app$snapshot()
