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
    # old mechanism through url
        query <- parseQueryString(session$clientData$url_search)
        print(query)
        if (!is.null(query[['example']])) {
         if (query[['example']] == "school") {
           updateSelectInput(session, "examplefile", selected = "mlmRev::Exam")
         }
        }
        if (input$examplefile %in% c("mlmRev::Exam", "lme4::sleepstudy")) {
            data <- eval(parse(text = input$examplefile))
            reactive$data <- data
            shinyjs::show("create_model")
            shinyjs::show("reactive_mode_area")
            shinyjs::hide("display_model")
            shinyjs::hide("output_region")
            id <- find_id(data)
            reactive$group_id_selected <- id[1]
            reactive$group_ids <- id
            result <- determine_levels(id[1], data, show_prog = T)
            reactive$level1 <- filter_ivs(result$level1, data)
            reactive$level2 <- filter_ivs(result$level2, data)
          }
        #}
    })
  output$file_area <- renderUI({
    accepted_filetype <- c("text/csv", "text/comma-separated-values",
                           "application/x-spss-sav", "application/x-spss-por",
                           "application/spss", ".sav", ".csv")
    fileInput("datafile", label = "1. Load your Data (.csv or .sav SPSS)",
              accept = accepted_filetype)
  })
  # read in data file, determine ID and level of variables----------------------
  observeEvent(input$datafile, {
    withProgress(message = "Loading data", value = 0, {
    req(input$datafile)
    shinyjs::show("create_model")
    shinyjs::show("reactive_mode_area")
    shinyjs::hide("display_model")
    shinyjs::hide("output_region")
    #shinyjs::hide("help")
    data <- load_data(input$datafile)
    reactive$data <- data

    id <- find_id(data)
    reactive$group_id_selected <- id[1]
    reactive$group_ids <- id
    result <- determine_levels(id[1], data, show_prog = T)
    reactive$level1 <- filter_ivs(result$level1, data)
    reactive$level2 <- filter_ivs(result$level2, data)
    })
  })
  # variable inputs are generated in the server file since they depend on
  # reactive values
  output$variables <- renderUI({
    if (length(reactive$group_id_selected) > 0) {
      fluidRow(
        column(width = 2, align = "center",
               selectInput("group_id", label = "Group ID",
                            selected = reactive$group_id_selected,
                            choices = reactive$group_ids),
               # button to calculate model
               # hide if reactive mode is on
               if (isolate(input$reactive_mode == TRUE)) {
               shinyjs::hidden(div(id = "start_calculation_button",
                                   actionButton("start_calculation_button",
                                                "Estimate model",
                                                width = "100%",
                                                icon = icon("calculator"))))
                 } else {
                 div(id = "start_calculation_button",
                     actionButton("start_calculation_button",
                                  "Estimate model",
                                  width = "100%",
                                  icon = icon("calculator")))
               }
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
    reactive$level1 <- filter_ivs(result$level1, reactive$data)
    reactive$level2 <- filter_ivs(result$level2, reactive$data)
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
    # try to isolate the inputs
    if (input$reactive_mode) {
      dv <- input$dv
      group_id <- input$group_id
      l1 <- input$l1
      l2 <- input$l2
      l1_varies <- input$l1_varies
      interaction <- input$interaction
      output_options <- input$output_options
    } else {
      if (input$start_calculation_button == 0) return()
      input$start_calculation_button
      dv <- isolate(input$dv)
      group_id <- isolate(input$group_id)
      l1 <- isolate(input$l1)
      l2 <- isolate(input$l2)
      l1_varies <- isolate(input$l1_varies)
      interaction <- isolate(input$interaction)
      output_options <- isolate(input$output_options)
    }
    # renderTable does not work if the object is empty, as is the case when
    # no dv and grouping var is selected, workaround:
    if (is.null(input$group_id) |
        is.null(dv) |
        !dv %in% names(reactive$data))
      # if you change data file, dv is not emptied
      return("Select dependent variable and grouping variable"
      )
    mdl_formula <- create_r_formula(dv, group_id, l1,
                                    l2, l1_varies,
                                    interaction)
    reactive$r_mdl_formula <- mdl_formula

    # calc the actual model
    showNotification("Estimating model...", id = "estimating_model",
                     duration = NULL, type = "message")
    mdl <- tryCatch({
      if (input$family != "gaussian") {
        lme4::glmer(stats::as.formula(mdl_formula), data = reactive$data,
                    nAGQ = input$nAGQ,
                    family = input$family)
      } else {
        lme4::lmer(stats::as.formula(mdl_formula), data = reactive$data)
      }
    },
      error = function(error_message){
        msg <- ifelse(grepl("<= number of random effects", error_message),
                      "Your model is unidentifiable. Try to reduce the number of random effects (e.g. remove variables from <<level 1 varies>>.)", error_message)
        shinyalert::shinyalert("Error", msg)
        message(error_message)
      }
    )
    reactive$table <- create_table(mdl, l1, output_options)
    output <- HTML(create_table(mdl, l1, output_options))
    removeNotification(id = "estimating_model")
    return(output)
  })
  #
  observeEvent(input$reactive_mode, {
      shinyjs::toggle("start_calculation_button")
  })
})
