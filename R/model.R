#' Detect a dichotomous response
#'
#' Missing values and unused factor levels are ignored.
#'
#' @param x response vector
#' @noRd
is_dichotomous_response <- function(x) {
  length(unique(x[!is.na(x)])) == 2
}

#' Ordered observed values of a dichotomous response
#'
#' Factor level order is respected. Other response values are sorted so the
#' second value can be used consistently as the modelled event.
#'
#' @param x response vector
#' @noRd
dichotomous_response_values <- function(x) {
  observed <- x[!is.na(x)]
  if (is.factor(x)) {
    present <- levels(droplevels(observed))
    return(present)
  }
  sort(unique(observed))
}

#' Choose the mixed-model family from the response
#'
#' @param x response vector
#' @noRd
response_family <- function(x) {
  if (length(unique(x[!is.na(x)])) == 2) {
    return("binomial")
  }
  if (is.numeric(x)) {
    return("gaussian")
  }
  NA_character_
}

#' Prepare data and family for automatic mixed-model fitting
#'
#' Dichotomous responses are converted to 0/1. The second sorted observed value
#' (or second observed factor level) is modelled as the event.
#'
#' @param data model data
#' @param dv dependent-variable name
#' @noRd
prepare_mixed_model_data <- function(data, dv) {
  family <- response_family(data[[dv]])
  if (is.na(family)) {
    stop("The dependent variable must be numeric or dichotomous.")
  }
  event <- NULL
  if (family == "binomial") {
    values <- dichotomous_response_values(data[[dv]])
    event <- values[2]
    data[[dv]] <- ifelse(
      is.na(data[[dv]]),
      NA_real_,
      as.numeric(data[[dv]] == event)
    )
  }
  list(data = data, family = family, event = event)
}

#' Fit an automatically selected mixed model
#'
#' @param formula model formula or formula string
#' @param data model data
#' @param dv dependent-variable name
#' @param nAGQ number of adaptive Gauss-Hermite quadrature points for GLMMs
#' @noRd
fit_mixed_model <- function(formula, data, dv, nAGQ = 1) {
  family <- response_family(data[[dv]])
  if (is.na(family)) {
    stop("The dependent variable must be numeric or dichotomous.")
  }
  event <- NULL
  if (family == "binomial") {
    values <- dichotomous_response_values(data[[dv]])
    event <- values[2]
    data[[dv]] <- ifelse(
      is.na(data[[dv]]),
      NA_real_,
      as.numeric(data[[dv]] == event)
    )
  }
  formula <- stats::as.formula(formula)
  if (family == "binomial") {
    model <- lme4::glmer(
      formula,
      data = data,
      nAGQ = nAGQ,
      family = stats::binomial()
    )
  } else {
    model <- lme4::lmer(formula, data = data)
  }
  attr(model, "mimosa_response_family") <- family
  attr(model, "mimosa_event") <- event
  model
}
