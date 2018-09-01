library(shiny)
library(shinydashboard)
library(lme4)
library(xtable)

shinyUI(

  dashboardPage(skin = "blue",
              
    dashboardHeader(title = "Mixed Models"),
    
    dashboardSidebar(
    
      tags$head(tags$style(HTML('.sidebar {padding-left: 8px;}'))),
      
      h4("Load data"),
      fileInput("datafile", label = NULL),
      helpText("Some options to correctly load data (e.g. variable names in first line, file type, separator, ...)")
    ),
  
    dashboardBody(

      fluidRow(

        # in this box variables are shown (sorted by levels) and can be chosen for the model
        box(title = "Variables", status = "primary", collapsible = T,
            
            uiOutput("variables")
        ),
        
        # in this box the model equations will be displayed
        box(title = "Model", status = "primary", collapsible = T,
            
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

