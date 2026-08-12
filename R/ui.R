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
acknowledgement_modal <- function(title, message) {
  modalDialog(
    title = title,
    message,
    easyClose = FALSE,
    footer = modalButton("OK")
  )
}

#' @noRd
mimosa_citation_plain <- function() {
  paste(
    "Titz, J. (2020). mimosa: A modern graphical user interface for",
    "2-level mixed models. Journal of Open Source Software, 5(49), 2116.",
    "https://doi.org/10.21105/joss.02116"
  )
}

#' @noRd
mimosa_citation_html <- function() {
  paste0(
    "<p>", mimosa_citation_html_body(), "</p>"
  )
}

#' @noRd
mimosa_citation_html_body <- function() {
  paste0(
    "Titz, J. (2020). mimosa: A modern graphical user interface for ",
    "2-level mixed models. <em>Journal of Open Source Software, 5</em>",
    "(49), 2116. <a href=\"https://doi.org/10.21105/joss.02116\">",
    "https://doi.org/10.21105/joss.02116</a>"
  )
}

#' @noRd
mimosa_citation_bibtex <- function() {
  paste(
    "@article{titz2020mimosa,",
    "  author = {Titz, Johannes},",
    "  title = {mimosa: A modern graphical user interface for 2-level mixed models},",
    "  journal = {Journal of Open Source Software},",
    "  year = {2020},",
    "  volume = {5},",
    "  number = {49},",
    "  pages = {2116},",
    "  doi = {10.21105/joss.02116}",
    "}",
    sep = "\n"
  )
}

#' @noRd
mimosa_version_label <- function() {
  version <- getOption("mimosa.version")
  if (is.null(version)) {
    version <- tryCatch(
      as.character(utils::packageVersion("mimosa")),
      error = function(e) "development"
    )
  }

  label <- paste0("mimosa v", version)
  if (isTRUE(getOption("mimosa.webR"))) {
    commit <- getOption("mimosa.commit")
    build <- if (is.null(commit) || !nzchar(commit)) "webR" else paste("webR", commit)
    label <- paste0(label, " (", build, ")")
  }
  label
}

#' @noRd
mimosa_version_title <- function() {
  label <- mimosa_version_label()
  if (!isTRUE(getOption("mimosa.webR"))) {
    return(label)
  }

  commit <- getOption("mimosa.commit")
  build <- if (is.null(commit) || !nzchar(commit)) "dev" else paste("dev", commit)
  tags$span(
    class = "mimosa-webr-version",
    style = "white-space: nowrap;",
    title = label,
    tags$span(
      class = "mimosa-webr-name",
      style = "font-size: 20px;",
      "mimosa"
    ),
    tags$span(
      class = "mimosa-webr-build",
      style = "font-size: 11px; margin-left: 6px;",
      build
    )
  )
}

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
     }
     .mimosa-citation-actions .btn {
       font-size: 12px;
       padding: 3px 7px;
     }"
  )),
  tags$script(HTML(
    "function mimosaCopyText(text, statusId, emptyMessage) {
       var status = document.getElementById(statusId);
       if (!text || !text.trim()) {
         status.textContent = emptyMessage;
         return;
       }
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
     }
     function mimosaCopyRCode() {
       var code = document.getElementById('r_analysis_code');
       mimosaCopyText(
         code ? code.textContent : '',
         'copy_r_code_status',
         'Estimate a model first.'
       );
     }
     function mimosaCopyCitation(format) {
       var citation = document.getElementById('mimosa_citation_' + format);
       mimosaCopyText(
         citation ? citation.value : '',
         'copy_citation_status',
         'Citation unavailable.'
       );
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
                width = NULL,
                uiOutput("table_region", class = "shiny-table-output"),
                br(),
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
                  <p>Latest release: <a href="https://github.com/johannes-titz/mimosa/releases/latest" target="_blank">mimosa 0.6.1 on GitHub</a></p>
                  <p>What is new? Read <a href="https://johannestitz.com/post/2026-08-11-mimosa-0-6-1/" target="_blank">the release blog post</a>.</p>
                  <p>Bugtracker: <a href="https://github.com/johannes-titz/mimosa/issues" target="_blank">https://github.com/johannes-titz/mimosa/issues</a></p>'),
            tags$p(
              style = "margin-bottom: 6px;",
              tags$strong("Citation:"),
              " ",
              HTML(mimosa_citation_html_body())
            ),
            tags$textarea(
              id = "mimosa_citation_plain",
              style = "display: none;",
              mimosa_citation_plain()
            ),
            tags$textarea(
              id = "mimosa_citation_html",
              style = "display: none;",
              mimosa_citation_html()
            ),
            div(
              class = "mimosa-code-actions mimosa-citation-actions",
              style = "margin-top: 0;",
              actionButton(
                "copy_citation_plain",
                "Copy plain text",
                icon = icon("copy"),
                onclick = "mimosaCopyCitation('plain');"
              ),
              actionButton(
                "copy_citation_html",
                "Copy HTML",
                icon = icon("copy"),
                onclick = "mimosaCopyCitation('html');"
              ),
              downloadButton("download_citation_bib", "Download BibTeX"),
              tags$span(
                id = "copy_citation_status",
                class = "text-muted",
                role = "status",
                style = "margin-left: 8px;"
              )
            ),
            HTML('<p>A good introduction to mixed models in German is available in <a href="https://www.pearson.de/datenanalyse-mit-r-fortgeschrittene-verfahren-9783868944136" target="_blank">Burkhardt, Titz, & Sedlmeier (2022)</a></p>
                  <p>If you want to support my work and/or you use R a lot, please check out the <a href="https://a.co/d/0ELTAQP" target="_blank">Essential R Cheatsheets</a>.</p>')))
  )
)
}

#' @importFrom shinydashboard dashboardPage dashboardHeader
#' @noRd
myui <- function() {
  testmode <- getOption("shiny.testmode")
  testmode <- ifelse(is.null(testmode), F, testmode)
  dashboardPage(
    skin = "red",
    header = dashboardHeader(title = mimosa_version_title()),
    # Sidebar-----------------------------------------------------------------
    sidebar = ui_sidebar,
    body = ui_body(testing = testmode),
  )
}
