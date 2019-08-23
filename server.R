shinyServer(function(input, output, session) {
  #bookmarking
  # observe({
  #   # Trigger this observer every time an input changes
  #   reactiveValuesToList(input)
  #   session$doBookmark()
  # })
  # onBookmarked(function(url) {
  #   updateQueryString(url)
  # })

  onBookmark(function(state){
    tempfile <- tempfile(fileext = ".Rdata")
    state$values$datafile <- tempfile
    saveRDS(reactive$data, tempfile)
  })
  onRestored(function(state){
    print(state$values$datafile)
    reactive$data <- readRDS(state$values$datafile)#load_data(state$values$datafile)
    id <- find_id(reactive$data)
    reactive$group_id_selected <- id[1]
    reactive$group_ids <- id
    result <- determine_levels(id[1], reactive$data)
    reactive$level1 <- result$level1
    reactive$level2 <- result$level2
  })
  
  # create reactive variables
  reactive <- reactiveValues(level1 = data.frame(), level2 = data.frame(),
                             data = data.frame(), r_mdl_formula = "",
                             group_id_selected = character(0), group_ids = character(0),
                             table = NULL)
  
  # read in data file, determine ID and level of variables----------------------
  observeEvent(input$datafile, {
    withProgress(message = "Loading data", value = 0, {
    req(input$datafile)
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
  # variable inputs are generated in the server file since they depend on reactive--
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
  
  output$download <- downloadHandler(
    filename = paste(Sys.Date(), "mimosa.html", sep = ""),
    content = function(file) {
      writeLines(reactive$table, file)
    }
  )

  observeEvent(input$group_id, {
    reactive$group_id_selected <- input$group_id
    result <- determine_levels(input$group_id, reactive$data)
    reactive$level1 <- result$level1
    reactive$level2 <- result$level2
  })

  # prevent selecting dv as predictor by removing it from choices----------
  observeEvent(input$dv, {
    sel <- input$l1[input$l1!=input$dv]
    updateCheckboxGroupInput(session, "l1",
      choiceNames = reactive$level1[reactive$level1 != input$dv],
      choiceValues = reactive$level1[reactive$level1 != input$dv],
      selected = sel)
  })
  # prevent selecting variation
  observeEvent(input$l1, {
    if (is.null(input$l1)) {
      updateCheckboxGroupInput(session, "interaction", choiceNames = "",
                               choiceValues = "",
                               selected = NULL)
    } else {
      updateCheckboxGroupInput(session, "l1_varies",
                               choiceNames = input$l1,
                               choiceValues = input$l1,
                               selected = input$l1_varies)

     interactions <- expand.grid(input$l1_varies, input$l2)
     print(interactions)
     if (ncol(interactions) == 2) {
       interactions <- paste(interactions[,1], interactions[,2], sep = ":")

    updateCheckboxGroupInput(session, "interaction",
                             choiceNames = interactions,
                             choiceValues = interactions,
                             selected = input$interaction)
                             }
    }
  }, ignoreNULL = F)

   observeEvent(input$l1_varies, {
    #  if (is.null(input$l1_varies)) {
    #   updateCheckboxGroupInput(session, "interaction", choiceNames = "",
    #                            choiceValues = "",
    #                            selected = NULL)
    # } else {
     interactions <- expand.grid(input$l1_varies, input$l2)
     if (ncol(interactions) == 2) {
       interactions <- paste(interactions[,1], interactions[,2], sep = ":")

       updateCheckboxGroupInput(session, "interaction",
                             choiceNames = interactions,
                             choiceValues = interactions,
                             selected = input$interaction)
     }
    # }
  }, ignoreNULL = F)


  observeEvent(input$l2, {
    if (is.null(input$l1) | is.null(input$l2)) {
      updateCheckboxGroupInput(session, "interaction", choiceNames = "",
                               choiceValues = "",
                               selected = NULL)
    } else {
    interactions <- expand.grid(input$l1_varies, input$l2)
    interactions <- paste(interactions[,1], interactions[,2], sep = ":")
    updateCheckboxGroupInput(session, "interaction",
                             choiceNames = interactions,
                             choiceValues = interactions,
                             selected = input$interaction)
    }
  }, ignoreNULL = F)


  # create HTML output for level 1 equation-------------------------------------
  output$mod_l1 <- renderUI({
    if (!is.null(input$dv)){
      equation <- c(input$dv, "<sub>ij</sub> = &beta;<sub>0j</sub> ",
                  " + e<sub>ij</sub>")
      if (!is.null(input$l1)){
        for (index in 1:length(input$l1)){
          equation <- append(equation,
                             c(" + &beta;<sub>", index, "j</sub>", input$l1[index],
                               "<sub>ij</sub>"),
                             after = length(equation)-1)
        }
      }
      HTML(paste(equation, collapse = ""))
    }
    else if (!is.null(input$l1)){
      HTML(paste("<p style=\"color:red\">Please select an dv variable first.</p>"))
    }
  })

  # create HTML output for level 2 equations------------------------------------
  output$mod_l2 <- renderUI({
    if (!is.null(input$dv)){
      equation <- c()
      if (1){
        eq_beta <- create_lvl2_constant(input$l2)
        for (l1_var in 0:length(input$l1)){
          if (l1_var > 0){
            l1_varies <- input$l1[l1_var] %in% input$l1_varies
            interaction <- NULL
            if (length(input$interaction) > 0){
              split <- strsplit(input$interaction, ":")
              logical <- grep(input$l1[l1_var], split)
              if (length(logical) > 0) {
                interaction <- sapply(split[logical], function(x) x[2])
              }
            }
            eq_beta <- create_mdl2_formula(l1_var, l1_varies, interaction)
          }
          equation <- append(equation, eq_beta)
        }
      }
      HTML(paste(equation, collapse = ""))
    }
    else if (!is.null(input$l2)){
      HTML(paste("<p style=\"color:red\">Please select an dv variable first.</p>"))
    }
  })

  output$mod_r <- renderUI({
    HTML("R formula")
    HTML(reactive$r_mdl_formula)
  })

  # create table ---------------------------------------------------------------
  output$table_region <- renderUI({
    # renderTable does not work if the object is empty, as is the case when
    # no iv and grouping var is selected, workaround:
    if (is.null(input$group_id) | is.null(input$dv))
      return("Select IV and grouping variable")
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
            shinyalert(sample(c("You deleted the Internet!","Run as fast as you can and don't look back.", "Catastrophic failure.", "User error - replace user.", "User error - It's not our fault"), 1), msg)
            message(error_message)
        }
      )
      mdl_smr <- summary(mdl)
      table <- mdl_smr$coefficients
      # add variances?
      show <- c("standard error", "p", "test statistic", "AIC",
               "Deviance", "Log-Likelihood") %in% input$output_options
      reactive$table <- tab_model(mdl, show.se = show[1], show.p = show[2], show.stat = show[3],
                     show.icc = TRUE, show.re.var = TRUE, show.ngroups = TRUE,
                     show.fstat = FALSE, show.aic = show[4], show.aicc = F,
                     show.dev = show[5], show.loglik = show[6],
                     string.se = "SE")[[3]]
      writeLines(tab_model(mdl, show.se = show[1], show.p = show[2], show.stat = show[3],
                     show.icc = TRUE, show.re.var = TRUE, show.ngroups = TRUE,
                     show.fstat = FALSE, show.aic = show[4], show.aicc = F,
                     show.dev = show[5], show.loglik = show[6],
                     string.se = "SE")[[3]], con = "output.html")
      HTML(tab_model(mdl, show.se = show[1], show.p = show[2], show.stat = show[3],
                     show.icc = TRUE, show.re.var = TRUE, show.ngroups = TRUE,
                     show.fstat = FALSE, show.aic = show[4], show.aicc = F,
                     show.dev = show[5], show.loglik = show[6],
                     string.se = "SE")[[3]])
    }

    #htmlOutput("table")
  })
})