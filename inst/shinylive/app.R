if (requireNamespace("webr", quietly = TRUE)) {
  webr::install(c(
    "dplyr",
    "Hmisc",
    "lme4",
    "mlmRev",
    "readr",
    "shiny",
    "shinybusy",
    "shinydashboard",
    "shinyjs",
    "sjPlot",
    "stringr"
  ))
}

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinybusy)
  library(shinyjs)
  library(dplyr)
  library(mlmRev)
  library(lme4)
  library(sjPlot)
  library(readr)
  library(stringr)
})

if (requireNamespace("Hmisc", quietly = TRUE)) {
  suppressPackageStartupMessages(library(Hmisc))
}

load("data/popular2.rda")
source("R/load.R", local = TRUE)
source("R/helper.R", local = TRUE)
source("R/formula.R", local = TRUE)
source("R/output.R", local = TRUE)
source("R/ui.R", local = TRUE)
source("R/server.R", local = TRUE)

shinyApp(ui = myui(), server = server)
