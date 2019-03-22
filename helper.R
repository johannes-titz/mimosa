extract_levels <- function(d, var, var_total_length){
  incProgress(1/var_total_length,
              message = paste("Testing Variable ", var, " as grouping variable"))
  # changed it so that simply the average number of levels for the var is
  # calculated
  d <- group_by_(d, var)
  get_levels <- function(x) {length(levels(as.factor(as.character(x))))}
  levels <- summarize_all(d, get_levels)
  levels[,1] <- NA
  mean(unlist(levels), na.rm = T)
}

find_id <- function(d){
  d <- select_if(d, function(x) is.integer(x) | is.character(x) | is.factor(x))
  # take only variables where every group has at least two values, this avoids
  # taking as a group id an arbitrary variable (e.g. open field) with many
  # possible values
  res <- apply(d, 2, function(x) prop.table(table(table(x) > 1))["TRUE"] == 1)
  vars <- names(d)[res]
  res <- sapply(vars, function(x) extract_levels(d, x, length(res)))
  position_of_id <- which.min(res)
  result <- vars[position_of_id]
  result
}

load_data <- function(datafile){
  fileending <- stringr::str_match(datafile$datapath, "(\\..+$)")[1,1]
  if (fileending == ".sav") {
    data <- Hmisc::spss.get(datafile$datapath, use.value.labels = F)
  }
  
  if (fileending == ".csv") {
    data <- read.csv(datafile$datapath)
  }
  data
}

identify_levels <- function(id_name, data){
  id <- unlist(data[, id_name])
  result <- NULL
  if (length(id) == 0) shinyalert("Error", "Cannot detect the ID variable. Check if your data is really hierarchical, please.", type = "error", callbackJS = "location.reload()")
  
  # find transition points between groups
  transition_points <- which(id != dplyr::lag(id))
  
  # check if all values until first transition point are equal
  # works only with values that are repeated; if only one value is here it does
  # not work
  equal <- apply(data[1:(transition_points[1]-1), ], 2, duplicated)
  if (is.null(dim(equal))) {
    equal <- apply(data[transition_points[1]:(transition_points[2]-1), ], 2, duplicated)}
  
  if (is.null(dim(equal))) shinyalert("Error", "I was not able to find at least two rows of data for the first group. Check if your data is really hierarchical, please.", type = "error", callbackJS = "location.reload()") # improve this
  equal <- apply(equal[2:dim(equal)[1], ], 2, all)
  # sort variables by levels
  # assuming a maximum of 2 levels
  result[[1]] <- data.frame(data[ ,!equal])
  equal[id_name] <- F
  result[[2]] <- select(data, which(equal))
  result
}

create_mdl2_formula <- function(beta_nmbr, beta_varies, interaction = NULL){
  beta_varies <- ifelse(beta_varies, paste(" + u<sub>", beta_nmbr, "j",
                                           "</sub>", sep =""), "")
  interaction <- ifelse(!is.null(interaction),
                        paste("&gamma;<sub>",
                              beta_nmbr, 1:length(interaction),
                              "</sub>", interaction, "<sub>j</sub>", 
                              collapse = "+", sep =""),
                        "")
  interaction <- ifelse(interaction == "", "", paste("+", interaction))
  eq_beta <- c("<br>&beta;<sub>", beta_nmbr, "j</sub> = &gamma;<sub>", beta_nmbr, 
                         "0</sub> ", interaction, beta_varies)
  eq_beta
}

create_lvl2_constant <- function(l2){
  part <- paste("&gamma;<sub>0", 1:length(l2), "</sub>", l2, "<sub>j</sub>",
                collapse = "+", sep ="")
  part <- paste("+", part)
  part2 <- paste("&beta;<sub>0j</sub> = &gamma;<sub>00</sub>", ifelse(is.null(l2), "", part))
  paste(part2, "+u<sub>0j</sub>")
}