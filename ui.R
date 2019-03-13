library(shiny)
library(shinydashboard)
library(lme4)
library(xtable)
library(sjPlot)
library(readr)
library(shinyalert)
library(Hmisc)
library(dplyr)
source("helper.R")

options(shiny.autoreload = F)

shinyUI(
  dashboardPage(skin = "blue",
    dashboardHeader(title = "mimosa"),
    dashboardSidebar(
      useShinyalert(),
      tags$head(tags$style(HTML('.sidebar {padding-left: 8px;}'))),
      h4("Load data"),
      fileInput("datafile", label = NULL),
      helpText("Some options to correctly load data (e.g. variable names in first line, file type, separator, ...)")
    ),
  
    dashboardBody(
      fluidRow(
        # in this box variables are shown (sorted by levels) and can be chosen for the model
        box(title = "Variables", status = "primary", collapsible = T, width = 7,
            uiOutput("variables")
        ),
        
        # in this box the model equations will be displayed
        box(title = "Model", status = "primary", collapsible = T, width = 5,
            strong("Level 1"), br(),
            uiOutput("mod_l1"),
            br(), strong("Level 2"),
            helpText("# lacks the possibility of choosing different predictors in each equation"),
            uiOutput("mod_l2")
        )
      ),
      fluidRow(
        # in this box the results will be shown
        box(title = "Output Table", status = "primary", width = 12,
        uiOutput("table_region")
        )
      )
    )
  )
)

