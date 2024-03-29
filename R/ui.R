#options(shiny.autoreload = T) # for faster testing, not stable
#options(shiny.sanitize.errors = T) # (handle errors manually)
enableBookmarking("url") # not currently supported, but maybe later

#' @importFrom shinydashboard dashboardSidebar
#' @importFrom shinybusy add_busy_spinner
#' @importFrom shinyjs hidden
#' @noRd
ui_sidebar <- shinydashboard::dashboardSidebar(
  shinybusy::add_busy_spinner(spin = "self-building-square",
                              position = "bottom-right",
                              margin = c(50, 0)),
  tags$head(tags$style(HTML('.sidebar {padding-left: 8px;}'))),
  # load data
  uiOutput("file_area"),
  selectInput("examplefile", "OR use example data sets:",
              c("", "Exam" = "mlmRev::Exam", "sleepstudy" = "lme4::sleepstudy",
                "Popularity2" = "mimosa::popular2"),
              selected = F, width = "150px"),
  shinyjs::hidden(tags$div(
    id = "reactive_mode_area",
    title = paste(
      "If the reactive mode is on, mimosa will recalculate the model",
      "after every change. If the reactive mode is off, you will need",
      "to manually click a button to recalculate the model."),
    checkboxInput(
      "reactive_mode",
      "Reactive mode",
      value = TRUE))),
  # footer----------------------------------------------------------------
  tags$style(
    type = 'text/css',
    "footer{position: absolute; bottom:1%; left: 5%; padding:5px;}"
  ),
  HTML(paste0('<footer><font size="1"><p style="color:grey">', Sys.Date(), '<br/>mimosa v0.5.0 &copy; Johannes Titz, license AGPL, Contributors: Maria Reichert<br><br>made with love and R:<br><ul style="color:grey"><li>shiny, shinydashboard, shinyalert for the interface</li><li>Hmisc for loading SPSS data</li><li>plyr and dplyr for data wrangling </li><li>lme4 for mixed model analysis</li><li>sjPlot for presentation</li></ul></p><p style="color:grey">feedback: mimosa@titz.science</p></font></footer>'))
)

#' @importFrom shinydashboard dashboardBody box
#' @importFrom shinyjs useShinyjs hidden extendShinyjs
#' @noRd
ui_body <- function(testing = F) {
  shinydashboard::dashboardBody(
  # shinytest2 does not react to shinyjs when called from command line, only
  # from rstudio, so we do not turn on shinyjs when in testing
  if (!testing) shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(script = "www/script.js", functions = c("collapse")),
  # Model spec and model display -----------------------------------------
  fluidRow(
    shinyjs::hidden(
      div(id = "create_model",
          box(title = "2. Create model", status = "primary",
              collapsible = T, width = 8, uiOutput("variables")
          ))),
    shinyjs::hidden(
      div(id = "display_model",
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
    shinyjs::hidden(
      div(id = "output_region",
          box(title = "3. Save output table", status = "primary",
              width = 6, uiOutput("table_region"), br(), 
              downloadButton("download", "Download Table")),
          
          # Table Options
          box(title = "Table Options", collapsed = T, status = "primary",
              collapsible = T, width = 2,
              checkboxGroupInput(
                "output_options",
                "Output options",
                choices = c(
                  "standard error",
                  "AIC",
                  "Deviance",
                  "Log-Likelihood",
                  "standardized coefficients",
                  "test statistic",
                  "p-value")
              )
          ),
          
          shinyjs::hidden(
            box(title = "Family",collapsed = T,
                status = "primary",
                collapsible = T, width = 2,
                radioButtons("family",
                             "Output options",
                             choices = c("gaussian",
                                         "binomial")
                )
            )),

          shinyjs::hidden(box(title = "Optimizer",collapsed = T,
                              status = "primary",
                              collapsible = T, width = 2,
                              numericInput("nAGQ", "Number of AGQ points", 1),
                              radioButtons("optimizer",
                                           "Output options",
                                           choices = c("Nelder_Mead",
                                                       "bobyqa")
                                           )
                              )                    
                           )
      ))),
  fluidRow(
    div(id = "help",
        box(title = "Help", status = "primary",
            collapsible = T,
            HTML('<p>How to use mimosa? See <a href="https://github.com/johannes-titz/mimosa/blob/master/README.md" target="_blank">README</a> for a short introduction.</p>
                  <p>Bugtracker: <a href="https://github.com/johannes-titz/mimosa/issues" target="_blank">https://github.com/johannes-titz/mimosa/issues</a></p>
                  <p>Citation: Titz, J. (2020). mimosa: A modern graphical user interface for 2-level mixed models. <i>Journal of Open Source Software, 5</i>(49), 2116. <a href ="https://doi.org/10.21105/joss.02116">https://doi.org/10.21105/joss.02116</a>
                  <p>A good introduction to mixed models in German is available in <a href="https://www.pearson.de/datenanalyse-mit-r-fortgeschrittene-verfahren-9783868944136" target="_blank">Burkhardt, Titz, & Sedlmeier (2022)</a></p>
                 <p>If you want to support my work and/or you use R a lot, please check out the <a href="https://a.co/d/0ELTAQP" target="_blank">Essential R Cheatsheets</a>.</p>')))
  )
)
}

#' @importFrom shinydashboard dashboardPage dashboardHeader
#' @importFrom shinyBS bsTooltip
#' @noRd
myui <- function() {
  testmode <- getOption("shiny.testmode")
  testmode <- ifelse(is.null(testmode), F, testmode)
  dashboardPage(
    skin = "red",
    header = dashboardHeader(title = "mimosa v0.5.1"),
    # Sidebar-----------------------------------------------------------------
    sidebar = ui_sidebar,
    body = ui_body(testing = testmode),
  )
}
