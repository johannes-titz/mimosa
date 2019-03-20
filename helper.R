extract_levels <- function(d, var){
  d <- group_by_(d, var)
  get_levels <- function(x) {length(levels(as.factor(as.character(x))))}
                
  levels <- summarize_all(d, get_levels)
  levels %>% summarize_all(get_levels)
}

find_id <- function(d){
  print(head(d))
  d <- select_if(d, function(x) is.integer(x) | is.character(x) | is.factor(x))
  vars <- names(d)
  res <- lapply(vars, function(x) extract_levels(d, x))
  
  df <- plyr::ldply(res, "data.frame")
  
  df <- filter_all(df, any_vars(. ==1))
  position_of_id <- which.max(apply(df, 1, max))
  result <- vars[position_of_id]
  result
}

load_data <- function(datafile){
  fileending <- stringr::str_match(datafile$datapath, "(\\..+$)")[1,1]
  if (fileending == ".sav") {
    data <- Hmisc::spss.get(datafile$datapath)
  }
  
  if (fileending == ".csv") {
    data <- readr::read_csv(datafile$datapath, col_types = NULL)
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
  
  if (is.null(dim(equal))) shinyalert("Error", "I was not able to find at least two rows of data for the first group. Check if your data is really hierarchical, please.", type = "error", callbackJS = "location.reload()") # improve this
  equal <- apply(equal[2:dim(equal)[1], ], 2, all)
  print(equal)
  
  # sort variables by levels
  # assuming a maximum of 2 levels
  result[[1]] <- data.frame(data[ , !equal])
  equal[id_name] <- F
  result[[2]] <- data.frame(data[ , equal])
  result
}

create_mdl2_formula <- function(beta_nmbr, beta_varies){
  beta_varies <- ifelse(beta_varies, paste(" + u<sub>", beta_nmbr, "j",
                                           "</sub>", sep =""), "")
  print(beta_varies)
  eq_beta <- c("<br>&beta;<sub>", beta_nmbr, "j</sub> = &gamma;<sub>", beta_nmbr, 
                         "0</sub> ", beta_varies)
  eq_beta
}
