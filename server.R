shinyServer(function(input, output, session) {
  # create reactive variables
  reactive <- reactiveValues(level1 = data.frame(),
                             level2 = data.frame(),
                             data = data.frame(), r_mdl_formula = "",
                             group_id_selected = character(0),
                             group_ids = character(0),
                             table = NULL)
  # read in data file, determine ID and level of variables----------------------
  observeEvent(input$datafile, {
    withProgress(message = "Loading data", value = 0, {
    req(input$datafile)
    shinyjs::show("create_model")
    shinyjs::hide("display_model")
    shinyjs::hide("output_region")
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
        !input$dv %in% names(reactive$data)) # if you change data file, input$dv is not emptied
      return("Select dependent variable and grouping variable")
    else {
      if (input$dv %in% input$l1) return()
      # renderTable(rownames = T,{
      fixed <- paste(c(input$l1, input$l2), collapse = "+")

      # random intercept model without any ivs
      if (fixed == ""){
        random_intercept <- paste("(1|", input$group_id, ")", sep = "")
      } else {

        # level does not vary
        random_intercept <- paste(input$l1_varies, collapse = "+")
        random_intercept <- paste("+(", random_intercept, "|", input$group_id, ")", sep = "")
        random_intercept <- ifelse(random_intercept == paste("+(|", input$group_id, ")", sep = ""),
                                   paste("+(1|", input$group_id, ")", sep = ""), random_intercept)
      }

      interaction <- paste(input$interaction, collapse = "+")
      mdl_formula <- paste(input$dv, "~",
                           fixed,
                           "+",
                           interaction,
                           random_intercept, sep = "")
      mdl_formula <- gsub("\\+\\+", "\\+", mdl_formula)
      mdl_formula <- gsub("\\~\\+", "\\~", mdl_formula)
      reactive$r_mdl_formula <- mdl_formula
      
      # calc the actual model
      mdl <- tryCatch({
        lmer(as.formula(mdl_formula), data = reactive$data)},
        error = function(error_message){
            msg <- ifelse(grepl("<= number of random effects", error_message),
                "Your model is unidentifiable. Try to reduce the number of random effects (e.g. remove variables from <<level 1 varies>>.)", error_message)
            shinyalert("Error", msg)
            message(error_message)
        }
      )

      # create the actual table
      show <- c("standard error", "AIC", "Deviance", "Log-Likelihood",
                "standardized coefficients", "test statistic", "p-value") %in% input$output_options
      if (length(input$l1) > 0 & show[7]){
        show_beta <- T
      } else {
        show_beta <- NULL
      }
      create_table <- function() {
        tab_model(mdl, show.se = show[1], show.p = show[2], show.stat = show[3],
                  show.icc = TRUE, show.re.var = TRUE, show.ngroups = TRUE,
                  show.fstat = FALSE, show.aic = show[4], show.aicc = F,
                  show.dev = show[5], show.loglik = show[6], string.se = "SE",
                  show.std = show_beta, string.std = "&beta;",
                  string.ci = "95% CI",
                  string.stat = "<i>t</i>",
                  collapse.ci = F)[[3]]
      }
      
      reactive$table <- create_table()
      HTML(create_table())
    }
  })
})