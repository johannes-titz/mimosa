#' Tries to find the grouping variable (id)
#' 
#' Works heuristically by (1) checking how many level-2-variables are present
#' for every potential grouping variable and (2) how many reverse levels are
#' present for every potential grouping variable.
#' 
#' @param d data frame
#' @return potential grouping variables
#' @import sjPlot
#' @import dplyr
#' @examples find_id(mlmRev::Exam)
#' @export
find_id <- function(d) {
  # first check data types and variation of variables
  d2 <- select_if(d, function(x) is_integer(x) | is.character(x) | is.factor(x))
  d2 <- select_if(d2, function(x) length(unique(x)) != length(x))
  d2 <- select_if(d2, function(x) length(unique(x)) != 1)
  # take only variables where a certain amount of groups has at least two
  # values, this avoids taking as a group id an arbitrary variable (e.g. open
  # field) with many possible values
  d2 <- select_if(d2, function(x) prop.table(table(table(x) > 1))["TRUE"] >= .60)
  variables <- names(d2)

  # sort variables by number of reverse levels
  avg_levels_mtrx <- create_avg_levels_mtrx(d2, variables)
  sum_of_reverse_levels <- colSums(avg_levels_mtrx, na.rm = T)
  # this gives the number of variables that are on level 2
  n_variables_lvl2 <- apply(avg_levels_mtrx, 1, function(x) sum(x == 1, na.rm = T))
  df <- data.frame(variables, n_variables_lvl2, sum_of_reverse_levels)
  # sort descending by number of lvl2 variables and by sum of reverse average
  # levels
  df <- df %>% 
    arrange(desc(n_variables_lvl2), desc(sum_of_reverse_levels), variables)
  df2 <- df %>% dplyr::filter(n_variables_lvl2 >= 1)
  group_variables <- as.character(df2$variables)
  if (length(group_variables) == 0) {
    group_variables <- as.character(df$variables)
  }
  # if still empty, there is probably no id
  if (length(group_variables) == 0) {
    showNotification("There seems to be no proper grouping variable (ID). The analysis will likely not work. Please check the structure of your data!",
                     duration = NULL)
    group_variables <- names(d)
  }
  group_variables
}

#' Determines variables on level 1 and level 2
#' 
#' By specifying the data and the group variable, the function will categorize
#' variables in level 1 and level 2.
#' 
#' @param group_variable name of the group variable (id) 
#' @param data yeah, this is the data
#' @param ignore_na whether to ignor na, default = T
#' @param show_prog whether to show progress in shiny app, default = F
#' @return list with two elements, consisting of the names of the variables on
#' each level
#' @examples determine_levels(find_id(mlmRev::Exam)[1], mlmRev::Exam)
#' @importFrom stats na.omit
#' @export
determine_levels <- function(group_variable, data, ignore_na = T,
                             show_prog = F){
  identified_levels <- find_avg_levels(data, group_variable, ncol(data),
                                       ignore_na, show_prog)
  result <- NULL
  level2 <- stats::na.omit(identified_levels == 1)
  result[[1]] <- names(level2)[!level2]
  level2[group_variable] <- F
  result[[2]] <- names(level2)[level2]
  names(result) <- c("level1", "level2")
  result
}

#' Number of levels
#' 
#' Check how many levels exist for a vector, adapted to the needs of mimosa.
#' 
#' @param x vector
#' @param ignore_na what is says, boolean, default = T
#' @return number of levels
#' @examples 
#' get_levels(c(NA, NA, NA))
#' get_levels(c(1, 1, 2, 3, 3, 5))
#' @noRd
get_levels <- function(x, ignore_na = TRUE) {
  length_levels <- length(levels(as.factor(as.character(x))))
  # if there are only NA in a variable, length_levels will be 0
  # see also example data by karin
  if (ignore_na) {
    length_levels <- ifelse(length_levels == 0, 1, length_levels)
  }
  length_levels
}

#' Calculate average number of levels
#' 
#' Takes the data frame and grouping variable and returns the average number
#' of levels over all groups for every variable
#' 
#' @param data data frame
#' @param group_var the grouping variable
#' @param var_total_length how many variables will be tested (only used for
#'   progress bar)
#' @param ignore_na yes, it is what is says, boolean, default = T
#' @param show_prog whether to show progress bar (useful for shiny and non-shiny
#'   use), default = F
#' @return vector with average levels per group for each variable of d
#' @noRd
find_avg_levels <- function(data, group_var, var_total_length, 
                            ignore_na = TRUE, show_prog = F){
  if (show_prog) {
    incProgress(1 / var_total_length, message = paste("Testing Variable ", group_var, " as grouping variable"))
  }
  data <- group_by_at(data, group_var)
  levels <- summarize_all(data, get_levels, ignore_na = ignore_na)
  levels[group_var] <- NA
  levels <- colMeans(levels)
  levels
}

#' Calculate average number of levels in matrix form
#' 
#' Takes the data frame and grouping variable and returns the average number
#' of levels over all groups for every variable in matrix form
#' 
#' @param data data frame
#' @param variables the variables that should be included
#' @return matrix with average levels per group for each potential grouping variable
#' @noRd
create_avg_levels_mtrx <- function(data, variables){
  avg_levels <- lapply(variables, function(x) t(find_avg_levels(data, x, length(variables))))
  avg_levels <- do.call(rbind.data.frame, avg_levels)
  rownames(avg_levels) <- variables
  avg_levels
}

#' Find moderators
#' 
#' @param var_name the variable to check for moderators
#' @param all_moderators all interaction terms
#' @return variables that moderate var_name
#' @noRd
who_moderates_me <- function(var_name, all_moderators){
  if (length(all_moderators) > 0){
    split <- strsplit(all_moderators, ":")
    position <- grep(var_name, split)
    if (length(position) > 0) {
      moderates_me <- sapply(split[position], function(x) x[2])
      return(moderates_me)
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
}

#' Filter independent variables
#' 
#' Options to filter out some independent variables. At the moment only nominal
#' variables with too many levels are filtered out in mimosa.
#' 
#' @param ivs the independent variables (character)
#' @param data the data frame
#' @param n_levels_max how many levels should a variable have at max
#' @noRd
filter_ivs <- function(ivs, data, n_levels_max = 10) {
  data <- data[names(data) %in% ivs]
  n_levels <- sapply(data, function(x) length(levels(x)))
  names(n_levels[(n_levels <= n_levels_max)])
}

#' check if all values of a vector are integers
#' 
#' @noRd
is_integer <- function(x) {
  ifelse(is.numeric(x), all(floor(x) == x, na.rm = TRUE), FALSE)
}