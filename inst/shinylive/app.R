suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyjs)
})

load("data/popular2.rda")
source("R/load.R", local = TRUE)
source("R/examples.R", local = TRUE)
source("R/helper.R", local = TRUE)
source("R/formula.R", local = TRUE)
source("R/model.R", local = TRUE)
source("R/output.R", local = TRUE)
source("R/ui.R", local = TRUE)
source("R/server.R", local = TRUE)

shinyApp(ui = myui(), server = server)
