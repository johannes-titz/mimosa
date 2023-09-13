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
