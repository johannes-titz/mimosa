d <- read.csv("hsball.csv", stringsAsFactors = F)

shinyServer(function(input, output, session) {
  # create reactive variables
  vars <- reactiveValues(level1 = data.frame(), level2 = data.frame(),
                         data = d, r_mdl_formula = "", group_id = character(0))
  
  # read in data file, determine ID and level of variables----------------------
  observeEvent(input$datafile, {
    req(input$datafile)
    data <- load_data(input$datafile)
    vars$data <- data
    
    id <- find_id(data)
    vars$group_id <- id
    
    ## identify levels (heuristically)
    
    # find transition points between groups
    # assuming that id is the first variable in the df
    
    if (length(id) == 0) shinyalert("Error", "Cannot detect the ID variable. Check if your data is really hierarchical, please.", type = "error", callbackJS = "location.reload()")
    
    row_counter <- c(2:dim(data)[1])
    print(row_counter)
    row_counter <- row_counter[data[row_counter, id] != data[row_counter-1, id]]
    print(row_counter)
    
    # check if all values until first transition point are equal
    # works only with values that are repeated; if only one value is here it does
    # not work
    equal <- apply(data[1:(row_counter[1]-1), ], 2, duplicated)
    print(equal)
    
    if (is.null(dim(equal))) shinyalert("Error", "I was not able to find at least two rows of data for the first group. Check if your data is really hierarchical, please.", type = "error", callbackJS = "location.reload()") # improve this
    equal <- apply(equal[2:dim(equal)[1], ], 2, all)
    print(equal)
    
    # sort variables by levels
    # assuming a maximum of 2 levels
    vars$level1 <- data.frame(data[ , !equal])
    equal[id] <- F
    vars$level2 <- data.frame(data[ , equal])
  })
  
  # variable inputs are generated in the server file since they depend on vars--
  output$variables <- renderUI({
    fluidRow(
      column(width = 2,
             radioButtons("group_id", label = "Group ID", selected = vars$group_id[1],
                          choices = vars$group_id)
      ),
      column(width = 2,
             radioButtons("outcome", label = "Outcome", selected = character(0),
                          choices = colnames(vars$level1))
      ),
      column(width = 2,
             checkboxGroupInput("l1", label = "Level 1", 
                                choiceNames = colnames(vars$level1),
                                choiceValues = colnames(vars$level1))
      ),
      conditionalPanel(condition = "input.l1.length > 0", 
      column(width = 2,
             checkboxGroupInput("l1_varies", label = "Level 1 varies")
      )),
      
      column(width = 2,
             checkboxGroupInput("l2", label = "Level 2", 
                                choiceNames = colnames(vars$level2),
                                choiceValues = colnames(vars$level2))
      ),
      conditionalPanel(condition = "input.l1.length > 0 & input.l2.length>0",
      column(width = 2,
             checkboxGroupInput("interaction", label = "Cross-level interaction")
      ))
    )
  })
  
  # prevent selecting outcome as predictor by removing it from choices----------
  observeEvent(input$outcome, {
    sel <- input$l1[input$l1!=input$outcome]
    print(sel)
    updateCheckboxGroupInput(session, "l1", 
      choiceNames = colnames(vars$level1)[colnames(vars$level1) != input$outcome],
      choiceValues = colnames(vars$level1)[colnames(vars$level1) != input$outcome],
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
                               selected = input$l1)
      
     interactions <- expand.grid(input$l1, input$l2)
     if (ncol(interactions) ==2) {
       interactions <- paste(interactions[,1], interactions[,2], sep = ":")
     
    updateCheckboxGroupInput(session, "interaction",
                             choiceNames = interactions,
                             choiceValues = interactions,
                             selected = input$interaction)
                             }
    }
  }, ignoreNULL = F)
  
  observeEvent(input$l2, {
    if (is.null(input$l1) | is.null(input$l2)) {
      updateCheckboxGroupInput(session, "interaction", choiceNames = "",
                               choiceValues = "",
                               selected = NULL)
    } else {
    interactions <- expand.grid(input$l1, input$l2)
    interactions <- paste(interactions[,1], interactions[,2], sep = ":")
    updateCheckboxGroupInput(session, "interaction",
                             choiceNames = interactions,
                             choiceValues = interactions,
                             selected = input$interaction)
    }
  }, ignoreNULL = F)
  
  
  # create HTML output for level 1 equation-------------------------------------
  output$mod_l1 <- renderUI({
    if (!is.null(input$outcome)){
      equation <- c(input$outcome, "<sub>ij</sub> = &beta;<sub>0j</sub> ",
                  " + e<sub>ij</sub>")
      if (!is.null(input$l1)){
        for (index in 1:length(input$l1)){
          equation <- append(equation, 
                             c(" + &beta;<sub>", index, "j</sub>", input$l1[index]), 
                             after = length(equation)-1)
        }
      }
      HTML(paste(equation, collapse = ""))
    }
    else if (!is.null(input$l1)){
      HTML(paste("<p style=\"color:red\">Please select an outcome variable first.</p>"))
    }
  })
  
  # create HTML output for level 2 equations------------------------------------
  # does not yet allow to modify equations independently 
  output$mod_l2 <- renderUI({
    if (!is.null(input$outcome)){
      equation <- c()
      if (1){
        for (l1_var in 0:length(input$l1)){
          if (l1_var > 0){
            eq_beta <- c("<br>&beta;<sub>", l1_var, "j</sub> = &gamma;<sub>", l1_var, 
                         "0</sub> ", " + u<sub>0j</sub>")
          }
          else {
            eq_beta <- c("&beta;<sub>", l1_var, "j</sub> = &gamma;<sub>", l1_var, 
                         "0</sub> ", " + u<sub>0j</sub>")
          }
            
          if (!is.null(input$l2)){
            for (l2_var in 1:length(input$l2)){
              eq_beta <- append(eq_beta,
                                c(" + &gamma;<sub>", l1_var, l2_var, "</sub>",
                                  input$l2[l2_var]),
                                after = length(eq_beta)-1)
            }
            equation <- append(equation, eq_beta)
          }
          else {
            equation <- append(equation, eq_beta)
          }
        }
      }
      HTML(paste(equation, collapse = ""))
    }
    else if (!is.null(input$l2)){
      HTML(paste("<p style=\"color:red\">Please select an outcome variable first.</p>"))
    }
    HTML("R formula")
    HTML(vars$r_mdl_formula)
  })
  
  # create table ---------------------------------------------------------------
  output$table_region <- renderUI({
    # renderTable does not work if the object is empty, as is the case when
    # no iv and grouping var is selected, workaround:
    if (is.null(input$group_id) | is.null(input$outcome))
      return("Select IV and grouping variable")
    else {
      if (input$outcome %in% input$l1) return()
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
      print(interaction)
      print(interaction)
      mdl_formula <- paste(input$outcome, "~",
                           fixed,
                           "+",
                           interaction, 
                           random_intercept, sep = "")
      mdl_formula <- gsub("\\+\\+", "\\+", mdl_formula)
      vars$r_mdl_formula <- mdl_formula
      # calc the actual model
      mdl <- lmer(as.formula(mdl_formula), data = vars$data)
      mdl_smr <- summary(mdl)
      table <- mdl_smr$coefficients
      # add variances?
      table
      #tab_model(mdl, file = "output.html")
      writeLines(tab_model(mdl)[[3]], con = "output.html")
      HTML(tab_model(mdl)[[3]])
    }

    #htmlOutput("table")
  })
})