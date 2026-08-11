#options(shiny.autoreload = T) # for faster testing, not stable
#options(shiny.sanitize.errors = T) # (handle errors manually)
enableBookmarking("url") # not currently supported, but maybe later

#' @importFrom shinydashboard dashboardSidebar
#' @importFrom shinyjs hidden
#' @noRd
ui_sidebar <- shinydashboard::dashboardSidebar(
  shiny::useBusyIndicators(),
  tags$head(tags$style(HTML('.sidebar {padding-left: 8px;}'))),
  # load data
  uiOutput("file_area"),
  uiOutput("examplefile_area"),
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
  HTML(paste0('<footer><font size="1"><p style="color:grey">', Sys.Date(), '<br/>mimosa &copy; Johannes Titz, license AGPL, Contributors: Maria Reichert<br><br>made with love and R:<br><ul style="color:grey"><li>shiny and shinydashboard for the interface</li><li>foreign for loading SPSS data</li><li>base R for data wrangling </li><li>lme4 for mixed model analysis</li><li>sjPlot for presentation</li></ul></p><p style="color:grey">feedback: mimosa@titz.science</p></font></footer>'))
)

#' @importFrom shinydashboard dashboardBody box
#' @importFrom shinyjs useShinyjs hidden
#' @noRd
ui_body <- function(testing = F) {
  shinydashboard::dashboardBody(
  # shinytest2 does not react to shinyjs when called from command line, only
  # from rstudio, so we do not turn on shinyjs when in testing
  if (!testing) shinyjs::useShinyjs(),
  tags$head(tags$style(HTML(
    ".mimosa-tooltip {
       position: relative;
       cursor: help;
       border-bottom: 1px dotted #777;
     }
     .mimosa-tooltip .mimosa-tooltip-box {
       visibility: hidden;
       opacity: 0;
       position: absolute;
       left: 0;
       bottom: 1.8em;
       width: 280px;
       z-index: 9999;
       padding: 8px 10px;
       border-radius: 4px;
       border: 1px solid #b7a200;
       background: #fff7b2;
       color: #111;
       font-size: 12px;
       line-height: 1.35;
       text-align: left;
       white-space: normal;
       transition: opacity 0.15s ease-in-out;
     }
     .mimosa-tooltip:hover .mimosa-tooltip-box,
     .mimosa-tooltip:focus .mimosa-tooltip-box,
     .mimosa-tooltip:focus-within .mimosa-tooltip-box {
       visibility: visible;
       opacity: 1;
     }
     .mimosa-r-code pre {
       max-height: 360px;
       max-width: 50ch;
       overflow-y: auto;
       white-space: pre-wrap;
       overflow-wrap: anywhere;
     }
     .mimosa-code-actions {
       margin: 8px 0;
     }"
  )),
  tags$script(HTML(
    "function mimosaCopyRCode() {
       var code = document.getElementById('r_analysis_code');
       var status = document.getElementById('copy_r_code_status');
       if (!code || !code.textContent.trim()) {
         status.textContent = 'Estimate a model first.';
         return;
       }
       var text = code.textContent;
       var copied = function(ok) {
         status.textContent = ok ? 'Copied.' : 'Copy failed.';
         window.setTimeout(function() { status.textContent = ''; }, 2500);
       };
       if (navigator.clipboard && window.isSecureContext) {
         navigator.clipboard.writeText(text).then(
           function() { copied(true); },
           function() { copied(false); }
         );
         return;
       }
       var area = document.createElement('textarea');
       area.value = text;
       area.style.position = 'fixed';
       area.style.opacity = '0';
       document.body.appendChild(area);
       area.select();
       var ok = document.execCommand('copy');
       document.body.removeChild(area);
       copied(ok);
     }"
  ))),
  # Model spec and model display -----------------------------------------
  fluidRow(
    column(
      width = 8,
      shinyjs::hidden(
        div(
          id = "create_model",
          box(
            title = "2. Create model", status = "primary",
            collapsible = T, width = NULL, uiOutput("variables")
          )
        )
      ),
      shinyjs::hidden(
        div(
          id = "output_region",
          fluidRow(
            column(
              width = 9,
              box(
                title = "3. Save output table", status = "primary",
                width = NULL, uiOutput("table_region"), br(),
                downloadButton("download", "Download Table")
              )
            ),
            column(
              width = 3,
              box(
                title = "Table Options", collapsed = T, status = "primary",
                collapsible = T, width = NULL,
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
                    "p-value"
                  )
                )
              ),
              shinyjs::hidden(
                box(
                  title = "Optimizer", collapsed = T, status = "primary",
                  collapsible = T, width = NULL,
                  numericInput("nAGQ", "Number of AGQ points", 1),
                  radioButtons(
                    "optimizer",
                    "Output options",
                    choices = c("Nelder_Mead", "bobyqa")
                  )
                )
              )
            )
          )
        )
      )
    ),
    column(
      width = 4,
      shinyjs::hidden(
        div(
          id = "display_model",
          box(title = "Model", status = "primary", collapsible = T, width = NULL,
              # level 1
              strong("Level 1"),
              br(),
              uiOutput("mod_l1"),
              # level 2
              br(), strong("Level 2"),
              uiOutput("mod_l2"),
              # model formula
              br(), strong("R model formula"),
              textOutput("mod_r")
          ),
          box(
              title = "R analysis code", status = "primary",
              collapsible = T, width = NULL,
              div(
                class = "mimosa-r-code",
                verbatimTextOutput("r_analysis_code", placeholder = TRUE)
              ),
              div(
                class = "mimosa-code-actions",
                actionButton(
                  "copy_r_code",
                  "Copy R code",
                  icon = icon("copy"),
                  onclick = "mimosaCopyRCode();"
                ),
                tags$span(
                  id = "copy_r_code_status",
                  class = "text-muted",
                  role = "status",
                  style = "margin-left: 8px;"
                )
              )
          )
        )
      )
    )
  ),
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
#' @noRd
myui <- function() {
  testmode <- getOption("shiny.testmode")
  testmode <- ifelse(is.null(testmode), F, testmode)
  mimosa_version <- tryCatch(
    utils::packageVersion("mimosa"),
    error = function(e) "webR"
  )
  dashboardPage(
    skin = "red",
    header = dashboardHeader(title = paste0("mimosa v", mimosa_version)),
    # Sidebar-----------------------------------------------------------------
    sidebar = ui_sidebar,
    body = ui_body(testing = testmode),
  )
}
