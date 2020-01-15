# Mimosa, the mixed models special agent, is a shiny app for 2-level mixed
# models.
#
# Copyright (C) 2019 Johannes Titz
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU Affero General Public License as published by the Free
# Software Foundation, either version 3 of the License, or any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
# details.
#
# You should have received a copy of the GNU Affero General Public License along
# with this program. If not, see <https://www.gnu.org/licenses/>.

options(shiny.autoreload = F) # for faster testing
options(shiny.sanitize.errors = FALSE) # (handle errors manually)
enableBookmarking("url") # not currently supported, but maybe later

#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar dashboardBody box
#' @importFrom shinyalert useShinyalert
#' @importFrom shinyjs useShinyjs hidden
#' @noRd
myui <- function(request){
shinyUI(
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "mimosa"),
      # Sidebar-----------------------------------------------------------------
      dashboardSidebar(
        tags$head(tags$style(HTML('.sidebar {padding-left: 8px;}'))),
        
        # load data
        h4("1. Load data"),
        fileInput("datafile", label = NULL,
                  accept = c("text/csv", "text/comma-separated-values",
                             "application/x-spss-sav", "application/x-spss-por",
                             "application/spss", ".sav", ".csv")),
        h6("Currently, you can only load .csv files and .sav (SPSS) files."),
        
        # footer
        tags$style(
          type = 'text/css',
          "footer{position: absolute; bottom:1%; left: 5%; padding:5px;}"
        ),
        HTML('<footer><font size="1"><p style="color:grey">&copy; 2019 Johannes Titz, license AGPL, Contributors: Maria Reichert<br><br>made with love and R:<br><ul style="color:grey"><li>shiny, shinydashboard, shinyalert for the interface</li><li>Hmisc for loading SPSS data</li><li>plyr and dplyr for data wrangling </li><li>lme4 for mixed model analysis</li><li>sjPlot for presentation</li></ul></p><p style="color:grey">feedback: mimosa@titz.science</p></font></footer>')
      ),
     dashboardBody(
       useShinyalert(), # for manual error handling, has to be in dashboardBody
        # Model spec and model display -----------------------------------------
        fluidRow(
          shinyjs::useShinyjs(),
          shinyjs::hidden(div(id = "create_model", 
          box(title = "2. Create model", status = "primary", collapsible = T,
              width = 8, uiOutput("variables")
          ))),
          shinyjs::hidden(div(id = "display_model",
            box(title = "Model", status = "primary", collapsible = T, width = 4,
                # level 1
                strong("Level 1"),
                br(),
                uiOutput("mod_l1"),
                # level 2
                br(), strong("Level 2"),
                uiOutput("mod_l2"),
                # model formula
                br(), strong("R model formula"),
                uiOutput("mod_r")
            ))
          )),
        # Output Table, Download -----------------------------------------------
       fluidRow(
         shinyjs::hidden(div(id = "output_region",
                    box(title = "3. Save output table",
                        status = "primary",
                        width = 6,
                        uiOutput("table_region"),
                        br(),
                        downloadButton("download", "Download Table")),
                    
                    # Table Options
                    box(title = "Table Options",collapsed = T,
                        status = "primary",
                        collapsible = T, width = 2,
                        checkboxGroupInput("output_options",
                                           "Output options",
                                           choices = c("standard error",
                                                       "AIC",
                                                       "Deviance",
                                                       "Log-Likelihood",
                                                       "standardized coefficients",
                                                       "test statistic",
                                                       "p-value")
                                           )
                        )
                    )
                )
         )
     )
  )
)
}
