shinyServer(function(input, output, session) {
  
  # initialize dataframes which will contain the variables
  # maybe they don't really need to be reactive
  level1 <- level2 <- data.frame()
  vars <- reactiveValues(level1 = level1, level2 = level2, data = data)
  
  observeEvent(input$datafile, {
    req(input$datafile)
    vars$data <- read.csv(file = input$datafile$datapath, stringsAsFactors = F)
    data <- vars$data
    # test file: "C:/HLM7 Student Examples/AppendxA/HSBALL.dat"
    
    ## identify levels (heuristically)
    
    # find transition points between groups
    # assuming that id is the first variable in the df
    row_counter <- c(2:dim(data)[1])
    row_counter <- row_counter[data[row_counter, 1] != data[row_counter-1, 1]]
    
    # check if all values until first transition point are equal
    equal <- apply(data[1:(row_counter[1]-1), ], 2, duplicated)
    equal <- apply(equal[2:dim(equal)[1], ], 2, all)
    
    # sort variables by levels
    # assuming a maximum of 2 levels
    vars$level1 <- data.frame(data[ , !equal])
    vars$level2 <- data.frame(data[ , equal])
    
  })
  
  # variable inputs are generated in the server file since they depend on vars
  output$variables <- renderUI({
    fluidRow(
      column(width = 3,
             radioButtons("group_id", label = "Group ID", selected = character(0),
                          choices = colnames(vars$level2))
      ),
      column(width = 3,
             radioButtons("outcome", label = "Outcome", selected = character(0),
                          choices = colnames(vars$level1))
      ),
      column(width = 3,
             checkboxGroupInput("l1", label = "Level 1", 
                                choiceNames = colnames(vars$level1),
                                choiceValues = colnames(vars$level1))
      ),
      column(width = 2,
             checkboxGroupInput("l1_varies", label = "Level 1 varies", 
                                choiceNames = colnames(vars$level1),
                                choiceValues = colnames(vars$level1))
      ),
      column(width = 3,
             checkboxGroupInput("l2", label = "Level 2", 
                                choiceNames = colnames(vars$level2),
                                choiceValues = colnames(vars$level2))
      )
    )
  })
  
  # prevent selecting outcome as predictor by removing it from choices
  observeEvent(input$outcome, {
    updateCheckboxGroupInput(session, "l1", 
                             choiceNames = colnames(vars$level1)[colnames(vars$level1) != input$outcome],
                             choiceValues = colnames(vars$level1)[colnames(vars$level1) != input$outcome],
                             selected = input$l1)
  })
  
  # create HTML output for level 1 equation
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
  
  # create HTML output for level 2 equations
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
      
  })
  
  output$table_region <- renderUI({
    # renderTable does not work if the object is empty, as is the case when
    # no iv and grouping var is selected, workaround:
    if (is.null(input$group_id) | is.null(input$outcome))
      return("Select IV and grouping variable")
    
    tableOutput("table")
  })
  
  output$table <- renderTable(rownames = T,{
    fixed <- paste(input$l1, collapse = "+")
    
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
    
    mdl_formula <- paste(input$outcome, "~", fixed, random_intercept, sep = "")
    # calc the actual model
    mdl <- lmer(as.formula(mdl_formula), data = vars$data)
    mdl_smr <- summary(mdl)
    table <- mdl_smr$coefficients
    table
  })
})

