extract_levels <- function(d, var){
  d <- group_by_(d, var)
  get_levels <- function(x) {length(levels(as.factor(as.character(x))))}
                
  levels <- summarize_all(d, get_levels)
  levels %>% summarize_all(get_levels)
}

find_id <- function(d){
  d <- dplyr::select_if(d, function(x) is.integer(x) | is.character(x) | is.factor(x))
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
}