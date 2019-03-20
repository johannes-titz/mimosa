library(shiny)
library(shinydashboard)
library(lme4)
library(xtable)
library(sjPlot)
library(readr)
library(shinyalert)
library(Hmisc)
library(dplyr)
library(clipr)
source("helper.R")

options(shiny.autoreload = F)

shinyUI(
  dashboardPage(skin = "blue",
    dashboardHeader(title = "mimosa"),
    dashboardSidebar(
      useShinyalert(),
      tags$head(tags$style(HTML('.sidebar {padding-left: 8px;}'))),
      h4("1. Load data"),
      fileInput("datafile", label = NULL),
      helpText("Currently, you can only load .csv files and .sav (SPSS) files.")
    ),
  
    dashboardBody(
      #conditionalPanel(condition = "typeof(reactive.group_id) != 'undefined'",
      fluidRow(
        # in this box variables are shown (sorted by levels) and can be chosen for the model
        box(title = "2. Create model", status = "primary", collapsible = T, width = 7,
            uiOutput("variables")
        ),
        
        conditionalPanel(condition = "input.outcome != undefined && input.outcome.length > 1",
        # in this box the model equations will be displayed
        box(title = "Model", status = "primary", collapsible = T, width = 5,
            strong("Level 1"), br(),
            uiOutput("mod_l1"),
            br(), strong("Level 2"),
            #helpText("# lacks the possibility of choosing different predictors in each equation"),
            uiOutput("mod_l2")
        ))
      ),
      
      fluidRow(
        conditionalPanel(condition = "input.outcome != undefined && input.outcome.length > 1",
        # in this box the results will be shown
        box(title = "3. Save output table", status = "primary", width = 12,
        uiOutput("table_region"),
        br(),
        downloadButton("download", "Download Table"))
      ))
    )
  )
)

