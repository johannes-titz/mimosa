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

#' @importFrom shinyjs show hide
#' @importFrom shinyalert shinyalert
#' @importFrom lme4 lmer
#' @import mlmRev
#' @importFrom stats as.formula
#' @noRd
myserver <- shinyServer(function(input, output, session) {
  # create reactive variables
  reactive <- reactiveValues(level1 = data.frame(),
                             level2 = data.frame(),
                             data = data.frame(), r_mdl_formula = "",
                             group_id_selected = character(0),
                             group_ids = character(0),
                             table = NULL)
  # example data set for tutorial in paper -------------------------------------
  observe({
        query <- parseQueryString(session$clientData$url_search)
        if (!is.null(query[['example']])) {
          if (query[['example']] == "school") {
            data <- mlmRev::Exam
            reactive$data <- data
            shinyjs::show("create_model")
            shinyjs::hide("display_model")
            shinyjs::hide("output_region")
            
            id <- find_id(data)
            reactive$group_id_selected <- id[1]
            reactive$group_ids <- id
            result <- determine_levels(id[1], data, show_prog = T)
            reactive$level1 <- result$level1
            reactive$level2 <- result$level2
          }
        }
    })
  output$file_area <- renderUI({
    if(!is.null(input$isSafari)){
      if (as.character(input$isSafari) == "TRUE") {
        accepted_filetype <- "*"
      } else {
        accepted_filetype <- c("text/csv", "text/comma-separated-values",
                               "application/x-spss-sav", "application/x-spss-por",
                               "application/spss", ".sav", ".csv")
      }
      fileInput("datafile", label = NULL, accept = accepted_filetype)
    }
  })
  # read in data file, determine ID and level of variables----------------------
  observeEvent(input$datafile, {
    withProgress(message = "Loading data", value = 0, {
    req(input$datafile)
    shinyjs::show("create_model")
    shinyjs::hide("display_model")
    shinyjs::hide("output_region")
    shinyjs::hide("help")
    data <- load_data(input$datafile)
    reactive$data <- data
    
    id <- find_id(data)
    reactive$group_id_selected <- id[1]
    reactive$group_ids <- id
    result <- determine_levels(id[1], data, show_prog = T)
    reactive$level1 <- result$level1
    reactive$level2 <- result$level2
    })
  })
  # variable inputs are generated in the server file since they depend on 
  # reactive values
  output$variables <- renderUI({
    if (length(reactive$group_id_selected) > 0) {
      fluidRow(
        column(width = 2,
               selectInput("group_id", label = "Group ID",
                            selected = reactive$group_id_selected,
                            choices = reactive$group_ids)
        ),
        column(width = 2,
               radioButtons("dv", label = "Dependent Variable",
                            selected = character(0),
                            choices = reactive$level1)
        ),
        column(width = 2,
               checkboxGroupInput("l1", label = "Level 1", 
                                  choiceNames = reactive$level1,
                                  choiceValues = reactive$level1)
        ),
        conditionalPanel(condition = "input.l1.length > 0", 
        column(width = 2,
               checkboxGroupInput("l1_varies", label = "Level 1 varies")
        )),
        column(width = 2,
               checkboxGroupInput("l2", label = "Level 2", 
                                  choiceNames = reactive$level2,
                                  choiceValues = reactive$level2)
        ),
        conditionalPanel(condition = "input.l1_varies.length > 0 & input.l2.length>0",
        column(width = 2,
               checkboxGroupInput("interaction",
                                  label = "Cross-level interaction",
                                  choiceNames = "",
                                  choiceValues = "")
        ))
      )
    }
  })
  
  # download table -------------------------------------------------------------
  output$download <- downloadHandler(
    filename = paste(Sys.Date(), "mimosa.html", sep = ""),
    content = function(file) {
      writeLines(reactive$table, file)
    }
  )
  
  # update levels, when group variable changes ---------------------------------
  observeEvent(input$group_id, {
    reactive$group_id_selected <- input$group_id
    result <- determine_levels(input$group_id, reactive$data)
    reactive$level1 <- result$level1
    reactive$level2 <- result$level2
  })

  # prevent selecting dv as predictor by removing it from choices --------------
  observeEvent(input$dv, {
    # note that input$l1 only gives selected inputs, not all possible inputs
    shinyjs::show("display_model")
    shinyjs::show("output_region")
    selected <- input$l1[input$l1 != input$dv]
    choices <- reactive$level1[reactive$level1 != input$dv]
    updateCheckboxGroupInput(session, "l1", choices = choices,
                             selected = selected)
  })
  # show level 1 random effects for selected level 1 variables -----------------
  observeEvent(input$l1, ignoreNULL = F, {
    # this could be simplified, but produces NA in checkbox during process
    # if no l1 variable, there can be no variation
    if (is.null(input$l1)) {
      updateCheckboxGroupInput(session, "l1_varies",
                               choices = character(0),
                               selected = input$l1_varies)
    } else {
      updateCheckboxGroupInput(session, "l1_varies",
                               choices = input$l1,
                               selected = input$l1_varies)
    }
  })
  # cross-level interaction when l1 random effect is selected-------------------
   observeEvent(input$l1_varies, ignoreNULL = F, {
     interactions <- expand.grid(input$l1_varies, input$l2)
     if (ncol(interactions) == 2) {
       interactions <- paste(interactions[,1], interactions[,2], sep = ":")
       updateCheckboxGroupInput(session, "interaction",
                                choices = interactions,
                                selected = input$interaction)
     }
  })
  # cross-level interaction when l2 variable is selected------------------------
  observeEvent(input$l2, ignoreNULL = F, {
    if (is.null(input$l2)) {
      updateCheckboxGroupInput(session, "interaction",
                               choices = "",
                               selected = NULL)
    } else {
    interactions <- expand.grid(input$l1_varies, input$l2)
    interactions <- paste(interactions[,1], interactions[,2], sep = ":")
    updateCheckboxGroupInput(session, "interaction",
                             choices = interactions,
                             selected = input$interaction)
    }
  })

  # create HTML output for level 1 equation-------------------------------------
  output$mod_l1 <- renderUI({
      HTML(create_equation(input$dv, input$l1))
  })

  # create HTML output for level 2 equations------------------------------------
  output$mod_l2 <- renderUI({
      equation <- create_lvl2_constant(input$l2)
      for (l1_var_pos in seq(input$l1)){
        l1_var <- input$l1[l1_var_pos]
        l1_varies <- l1_var %in% input$l1_varies
        interaction <- who_moderates_me(l1_var, input$interaction)
        eq_beta <- create_mdl2_formula(l1_var_pos, l1_varies, interaction)
        equation <- c(equation, eq_beta)
      }
      HTML(paste(equation, collapse = ""))
  })
  # create R model formula -----------------------------------------------------
  output$mod_r <- renderUI({
    HTML("R formula")
    HTML(reactive$r_mdl_formula)
  })

  # create table ---------------------------------------------------------------
  output$table_region <- renderUI({
    # renderTable does not work if the object is empty, as is the case when
    # no dv and grouping var is selected, workaround:
    if (is.null(input$group_id) |
        is.null(input$dv) |
        !input$dv %in% names(reactive$data))
      # if you change data file, input$dv is not emptied
      return("Select dependent variable and grouping variable"
      )
    mdl_formula <- create_r_formula(input$dv, input$group_id, input$l1,
                                    input$l2, input$l1_varies,
                                    input$interaction)
    reactive$r_mdl_formula <- mdl_formula
    
    # calc the actual model
    mdl <- tryCatch({
      lme4::lmer(stats::as.formula(mdl_formula), data = reactive$data)},
      error = function(error_message){
        msg <- ifelse(grepl("<= number of random effects", error_message),
                      "Your model is unidentifiable. Try to reduce the number of random effects (e.g. remove variables from <<level 1 varies>>.)", error_message)
        shinyalert::shinyalert("Error", msg)
        message(error_message)
      }
    )
    
    reactive$table <- create_table(mdl, input$l1, input$output_options)
    HTML(create_table(mdl, input$l1, input$output_options))
  })
})