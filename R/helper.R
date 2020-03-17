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

#' Tries to identify the grouping variable (id)
#' 
#' Works heuristically by (1) checking how many level-2-variables are present
#' for every potential grouping variable and (2) how many reverse levels are
#' present for every potential grouping variable.
#' 
#' @param d data frame
#' @return potential grouping variables
#' @import sjPlot
#' @import dplyr
#' @importFrom plyr ldply
#' @examples find_id(mlmRev::Exam)
#' @noRd
find_id <- function(d){
  # first check data types and variation of variables
  d2 <- select_if(d, function(x) is.integer(x) | is.character(x) | is.factor(x))
  d2 <- select_if(d2, function(x) length(unique(x)) != length(x))
  d2 <- select_if(d2, function(x) length(unique(x)) != 1)
  # take only variables where a certain amount of groups has at least two
  # values, this avoids taking as a group id an arbitrary variable (e.g. open
  # field) with many possible values
  d2 <- select_if(d2, function(x) prop.table(table(table(x) > 1))["TRUE"] >= .70)
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
  df2 <- df %>% filter(n_variables_lvl2 >= 1)
  group_variables <- as.character(df2$variables)
  if (length(group_variables) == 0) {
    group_variables <- as.character(df$variables)
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
#' @noRd
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
  data <- group_by_(data, group_var)
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
  avg_levels <- plyr::ldply(avg_levels, data.frame)
  rownames(avg_levels) <- variables
  avg_levels
}

#' Load the data
#' 
#' Depending on file ending, the data is loaded.
#' 
#' @importFrom Hmisc spss.get
#' @importFrom utils read.csv read.csv2 count.fields
#' @importFrom readr guess_encoding
#' @importFrom stringr str_match
#' @param datafile yep, the data file to upload
#' @return data as an R object or an error
#' @noRd
load_data <- function(datafile){
  fileending <- stringr::str_match(datafile$datapath, "(\\..+$)")[1,1]
  data <- tryCatch({
    if (fileending == ".sav") {
      data <- Hmisc::spss.get(datafile$datapath, use.value.labels = F)
    } else if (fileending == ".csv") {
      encoding <- unlist(readr::guess_encoding(datafile$datapath)[1, 1])
      L <- readLines(datafile$datapath, n = 1)
      numfields_semicolon <- count.fields(textConnection(L), sep = ";")
      numfields_colon <- count.fields(textConnection(L), sep = ",")
      if (numfields_semicolon == 1) {
        data <- utils::read.csv(datafile$datapath, fileEncoding = encoding)
      } else if (numfields_colon == 1) {
        data <- utils::read.csv2(datafile$datapath, fileEncoding = encoding)
      }
    }
    data},
    error = function(error_message){
      msg <- "Sorry, I could not read your data. Please check that it is in the SPSS format .sav or a regular .csv file with a comma as a separator (not a semicolon or any other delimiter)."
      shinyalert::shinyalert("Error", msg)
      message(error_message)
    }
    )
}

# this should go in separate file ---------
#' Create model formula for level 2
#' 
#' This produces the model output for level 2
#' @param beta_nmbr index of effect
#' @param beta_varies whether the effect is random
#' @param interaction whether there is an interaction for the specific beta
#' @return the formula as html
#' @noRd
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
  eq_beta <- c("<br>&beta;<sub>", beta_nmbr, "j</sub> = &gamma;<sub>",
               beta_nmbr, "0</sub> ", interaction, beta_varies)
  eq_beta
}

#' Create model formula for level 2 constant
#' 
#' @param l2 variables on level 2
#' @return the formula for the constant as html (Beta_0j)
#' @noRd
create_lvl2_constant <- function(l2){
  part <- paste("&gamma;<sub>0", 1:length(l2), "</sub>", l2, "<sub>j</sub>",
                collapse = "+", sep ="")
  part <- paste("+", part)
  part2 <- paste("&beta;<sub>0j</sub> = &gamma;<sub>00</sub>",
                 ifelse(is.null(l2), "", part))
  paste(part2, "+u<sub>0j</sub>")
}

#' Create model formula for one slope
#' 
#' @param var_name the variable name
#' @param position the position of the variable
#' @return the formula for one slope as html
#' @noRd
create_one_slope <- function(var_name, position){
  paste(" + &beta;<sub>", position, "j</sub>", var_name,
        "<sub>ij</sub>", sep = "")
}

#' Create equation
#' 
#' @param dv dependent variable
#' @param l1 level 1 variables
#' @noRd
create_equation <- function(dv, l1 = NULL){
  slopes <- paste(mapply(create_one_slope, l1, seq(l1)), collapse = "")
  constant <- " &beta;<sub>0j</sub>"
  error <- " + e<sub>ij</sub>"
  left_side <- paste(dv, "<sub>ij</sub> =", sep ="")
  right_side <- paste(constant, slopes, error, sep ="")
  equation <- paste(left_side, right_side, sep = "")
  equation
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

#' Create r formula
#' 
#' @param dv dependent variable
#' @param group_id grouping variable
#' @param l1 level 1 variables
#' @param l2 level 2 variables
#' @param l1_varies level 1 random variables
#' @param interaction interaction terms
#' @return the r model formula for lme4
#' @noRd
create_r_formula <- function(dv, group_id, l1 = NULL, l2 = NULL,
                             l1_varies = NULL, interaction = NULL){
  fixed <- paste(c(l1, l2), collapse = "+")
  
  # random intercept model without any ivs
  if (fixed == ""){
    random_intercept <- paste("(1|", group_id, ")", sep = "")
  } else {
    
    # level does not vary
    random_intercept <- paste(l1_varies, collapse = "+")
    random_intercept <- paste("+(", random_intercept, "|", group_id, ")",
                              sep = "")
    random_intercept <- ifelse(random_intercept == paste("+(|", group_id, ")",
                                                         sep = ""),
                               paste("+(1|", group_id, ")", sep = ""),
                               random_intercept)
  }
  
  interaction <- paste(interaction, collapse = "+")
  mdl_formula <- paste(dv, "~",
                       fixed,
                       "+",
                       interaction,
                       random_intercept, sep = "")
  mdl_formula <- gsub("\\+\\+", "\\+", mdl_formula)
  mdl_formula <- gsub("\\~\\+", "\\~", mdl_formula)
  mdl_formula
}

#' Create table output
#' 
#' This simply uses sjPlot with some adapted defaults.
#' 
#' @importFrom sjPlot tab_model
#' @param mdl the lme4 model object
#' @param l1 level 1 variables
#' @param output_options what to show in the output
#' @noRd
create_table <- function(mdl, l1, output_options){
  check <- c("standard error", "AIC", "Deviance", "Log-Likelihood",
             "standardized coefficients", "test statistic", "p-value") 
  
  show <- check %in% output_options
  names(show) <- check
  
  if (length(l1) > 0 & show["standardized coefficients"]){
    show_beta <- T
  } else {
    show_beta <- NULL
  }
  sjPlot::tab_model(mdl,
                    show.se = show["standard error"], 
                    show.p = show["p-value"],
                    show.stat = show["test statistic"],
                    show.aic = show["AIC"],
                    show.dev = show["Deviance"],
                    show.loglik = show["Log-Likelihood"],
                    show.std = show_beta,
                    string.se = "SE",
                    string.std = "&beta;",
                    string.ci = "95% CI",
                    string.stat = "<i>t</i>",
                    collapse.ci = F, show.icc = TRUE, show.re.var = TRUE,
                    show.ngroups = TRUE, show.fstat = FALSE, show.aicc = F)[[3]]
}

#' Filter independent variables
#' 
#' Options to filter out some independent variables. At the moment only nominal
#' variables with too many levels are filtered out.
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