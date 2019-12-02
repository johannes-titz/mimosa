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

#' @import sjPlot
#' @import dplyr
#' @importFrom plyr ldply
#' @noRd
find_id <- function(d){
  # first check data types and variation of variables
  d2 <- select_if(d, function(x) is.integer(x) | is.character(x) | is.factor(x))
  d2 <- select_if(d2, function(x) length(unique(x)) != length(x))
  d2 <- select_if(d2, function(x) length(unique(x)) != 1)
  # take only variables where every group has at least two values, this avoids
  # taking as a group id an arbitrary variable (e.g. open field) with many
  # possible values
  d2 <- select_if(d2, function(x) prop.table(table(table(x) > 1))["TRUE"] >= .70)
  vars <- names(d2)
  # for every potential grouping variable, check how many level-2 vars it
  # would create
  res <- lapply(vars, function(x) t(extract_levels2(d2, x, length(vars))))
  res <- plyr::ldply(res, data.frame)
  rownames(res) <- vars
  # this gives the number of variables that are on level 2
  test2 <- apply(res, 1, function(x) sum(x == 1, na.rm = T))
  test2 <- test2[test2 > 0]
  ids <- names(sort(test2, decreasing = T))
  if (length(ids) == 0){
    ids <- vars
  }
  ids
}

#' @importFrom stats na.omit
#' @noRd
determine_levels <- function(id_name, data, ignore_na = T, show_prog = F){
  identified_levels <- extract_levels2(data, id_name, ncol(data), ignore_na, show_prog)
  result <- NULL
  level2 <- stats::na.omit(identified_levels == 1)
  result[[1]] <- names(level2)[!level2]
  level2[id_name] <- F
  result[[2]] <- names(level2)[level2]
  names(result) <- c("level1", "level2")
  result
}

get_levels <- function(x, ignore_na = TRUE) {
  length_levels <- length(levels(as.factor(as.character(x))))
  # if there are only NA in a variable, length_levels will be 0
  # see also example data by karin
  if (ignore_na) {
    length_levels <- ifelse(length_levels == 0, 1, length_levels)
  }
  length_levels
}

extract_levels2 <- function(d, var, var_total_length, 
                            ignore_na = TRUE, show_prog = F){
  if(show_prog) {
    incProgress(1 / var_total_length, message = paste("Testing Variable ", var, " as grouping variable"))
  }
  d <- group_by_(d, var)
  levels <- summarize_all(d, get_levels, ignore_na = ignore_na)
  levels[var] <- NA
  levels <- colMeans(levels)
  levels
}

#' @importFrom Hmisc spss.get
#' @importFrom utils read.csv read.csv2 count.fields
#' @importFrom readr guess_encoding
#' @importFrom stringr str_match
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

create_lvl2_constant <- function(l2){
  part <- paste("&gamma;<sub>0", 1:length(l2), "</sub>", l2, "<sub>j</sub>",
                collapse = "+", sep ="")
  part <- paste("+", part)
  part2 <- paste("&beta;<sub>0j</sub> = &gamma;<sub>00</sub>",
                 ifelse(is.null(l2), "", part))
  paste(part2, "+u<sub>0j</sub>")
}

create_one_slope <- function(var_name, position){
  paste(" + &beta;<sub>", position, "j</sub>", var_name,
        "<sub>ij</sub>", sep = "")
}

create_equation <- function(dv, l1 = NULL){
  slopes <- paste(mapply(create_one_slope, l1, seq(l1)), collapse = "")
  constant <- " &beta;<sub>0j</sub>"
  error <- " + e<sub>ij</sub>"
  left_side <- paste(dv, "<sub>ij</sub> =", sep ="")
  right_side <- paste(constant, slopes, error, sep ="")
  equation <- paste(left_side, right_side, sep = "")
  equation
}

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

#' @importFrom sjPlot tab_model
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
