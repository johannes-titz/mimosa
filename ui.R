library(shiny)
library(shinydashboard)
library(lme4)
#library(xtable)
library(sjPlot)
#library(readr)
library(shinyalert)
#library(Hmisc)
library(dplyr)
source("helper.R")
options(shiny.autoreload = F)

options(shiny.sanitize.errors = FALSE)

shinyUI(

  dashboardPage(skin = "blue",
                
    dashboardHeader(title = "mimosa"),
    dashboardSidebar(
      tags$style(type = 'text/css', 
               "footer{position: absolute; bottom:1%; left: 5%; padding:5px;}"
      ),
      useShinyalert(),
      tags$head(tags$style(HTML('.sidebar {padding-left: 8px;}'))),
      h4("1. Load data"),
      fileInput("datafile", label = NULL),
      h6("Currently, you can only load .csv files and .sav (SPSS) files."),
      HTML('<footer><font size="1"><p style="color:grey">&copy; 2019 Johannes Titz, license AGPL, Contributors: Maria Reichert<br><br>made with love and R:<br><ul style="color:grey"><li>shiny, shinydashboard, shinyalert for the interface</li><li>Hmisc for loading SPSS data</li><li>plyr and dplyr for data wrangling </li><li>lme4 for mixed model analysis</li><li>sjPlot for presentation</li></ul></p><p style="color:grey">feedback: mimosa@titz.science</p></font></footer>')
    ),
  
    dashboardBody(
      fluidRow(
        #conditionalPanel(condition = "output.fileUploaded",
        # in this box variables are shown (sorted by levels) and can be chosen for the model
        box(title = "2. Create model", status = "primary", collapsible = T, width = 8,
            uiOutput("variables")
        ),
        
        conditionalPanel(condition = "input.outcome != undefined && input.outcome.length > 1",
        # in this box the model equations will be displayed
        box(title = "Model", status = "primary", collapsible = T, width = 4,
            strong("Level 1"), br(),
            uiOutput("mod_l1"),
            br(), strong("Level 2"),
            uiOutput("mod_l2"),
            #helpText("# lacks the possibility of choosing different predictors in each equation"),
            br(), strong("R model formula"),
            uiOutput("mod_r")
        ))
      ),
      
      fluidRow(
        conditionalPanel(condition = "input.outcome != undefined && input.outcome.length > 1",
        # in this box the results will be shown
        box(title = "3. Save output table", status = "primary", width = 8,
        uiOutput("table_region"),
        br(),
        downloadButton("download", "Download Table"))
      ))
    )
  )
)

