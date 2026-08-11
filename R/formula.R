#' Create model formula for level 2
#' 
#' This produces the model output for level 2
#' @param beta_nmbr index of effect
#' @param beta_varies whether the effect is random
#' @param interaction whether there is an interaction for the specific beta
#' @return the formula as html
#' @noRd
create_mdl2_formula <- function(beta_nmbr, beta_varies, interaction = NULL) {
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
create_lvl2_constant <- function(l2) {
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
create_one_slope <- function(var_name, position) {
  paste(" + &beta;<sub>", position, "j</sub>", var_name,
        "<sub>ij</sub>", sep = "")
}

#' Create equation
#' 
#' @param dv dependent variable
#' @param l1 level 1 variables
#' @noRd
create_equation <- function(dv, l1 = NULL) {
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
                             l1_varies = NULL, interaction = NULL) {
  fixed <- vapply(c(l1, l2), quote_r_name, character(1))
  interactions <- vapply(
    interaction,
    quote_r_interaction,
    character(1)
  )
  random_slopes <- vapply(l1_varies, quote_r_name, character(1))
  random_terms <- paste(c("1", random_slopes), collapse = " + ")
  random <- paste0("(", random_terms, " | ", quote_r_name(group_id), ")")
  rhs <- c(fixed, interactions, random)

  paste(quote_r_name(dv), "~", paste(rhs, collapse = " + "))
}

#' Quote a variable name for use in an R formula
#'
#' @param name variable name
#' @noRd
quote_r_name <- function(name) {
  paste(deparse(as.name(name), backtick = TRUE), collapse = "")
}

#' Quote the variable names in an interaction term
#'
#' @param term interaction term separated by a colon
#' @noRd
quote_r_interaction <- function(term) {
  variables <- strsplit(term, ":", fixed = TRUE)[[1]]
  paste(vapply(variables, quote_r_name, character(1)), collapse = ":")
}

#' Create copyable R code for the complete analysis
#'
#' The generated code follows the same response-family selection, event coding,
#' model fitting, and variance decomposition used by Mimosa.
#'
#' @param formula model formula
#' @param data_code R code that loads the selected data into `data`
#' @param dv dependent-variable name
#' @param family either `gaussian` or `binomial`
#' @param event modelled event for a binomial response
#' @param nAGQ number of adaptive Gauss-Hermite quadrature points
#' @noRd
create_analysis_code <- function(formula, data_code, dv, family,
                                 event = NULL, nAGQ = 1) {
  stopifnot(family %in% c("gaussian", "binomial"))
  r_code_tokens <- function(code) {
    characters <- strsplit(code, "", fixed = TRUE)[[1]]
    tokens <- character(0)
    token <- ""
    in_backticks <- FALSE
    for (character in characters) {
      if (identical(character, "`")) {
        in_backticks <- !in_backticks
      }
      if (identical(character, " ") && !in_backticks) {
        if (nzchar(token)) {
          tokens <- c(tokens, token)
          token <- ""
        }
      } else {
        token <- paste0(token, character)
      }
    }
    c(tokens, token[nzchar(token)])
  }
  format_formula_argument <- function(formula, width = 50) {
    tokens <- r_code_tokens(formula)
    prefix <- "  formula ="
    continuation <- "    "
    current <- prefix
    formatted <- character(0)

    for (token in tokens) {
      candidate <- paste(current, token)
      if (nchar(candidate) <= width || identical(current, prefix)) {
        current <- candidate
      } else if (token %in% c("+", "~", "|")) {
        formatted <- c(formatted, paste(current, token))
        current <- continuation
      } else {
        formatted <- c(formatted, current)
        current <- paste(continuation, token)
      }
    }
    paste(c(formatted, paste0(current, ",")), collapse = "\n")
  }
  lines <- c("# Load data", data_code, "")

  if (family == "binomial") {
    dv_literal <- r_value_literal(dv)
    event_literal <- r_value_literal(event)
    lines <- c(
      lines,
      paste0("# Model ", event_literal, " as the event (1)"),
      paste0("data[[", dv_literal, "]] <- ifelse("),
      paste0("  is.na(data[[", dv_literal, "]]),"),
      "  NA_real_,",
      "  as.numeric(",
      paste0("    data[[", dv_literal, "]] == ", event_literal),
      "  )",
      ")",
      "",
      "# Fit the binomial mixed model",
      "model <- lme4::glmer(",
      format_formula_argument(formula),
      "  data = data,",
      "  family = stats::binomial(),",
      paste0("  nAGQ = ", as.integer(nAGQ)),
      ")"
    )
  } else {
    lines <- c(
      lines,
      "# Fit the Gaussian mixed model",
      "model <- lme4::lmer(",
      format_formula_argument(formula),
      "  data = data",
      ")"
    )
  }

  paste(c(
    lines,
    "",
    "# Display the model table",
    "sjPlot::tab_model(model)"
  ), collapse = "\n")
}

#' Convert a scalar R value to copyable source code
#'
#' @param value scalar value
#' @noRd
r_value_literal <- function(value) {
  paste(deparse(value, control = "all"), collapse = "")
}
