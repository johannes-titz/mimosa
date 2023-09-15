#' Create table output
#' 
#' This simply uses sjPlot with some adapted defaults.
#' 
#' @importFrom sjPlot tab_model
#' @param mdl the lme4 model object
#' @param l1 level 1 variables
#' @param output_options what to show in the output
#' @noRd
create_table <- function(mdl, l1, output_options) {
  check <- c("standard error", "AIC", "Deviance", "Log-Likelihood",
             "standardized coefficients", "test statistic", "p-value") 
  
  show <- check %in% output_options
  names(show) <- check
  
  if (length(l1) > 0 & show["standardized coefficients"]) {
    show_beta <- T
  } else {
    show_beta <- NULL
  }
  sjPlot::tab_model(
    mdl,
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
    show.ngroups = TRUE, show.fstat = FALSE, show.aicc = F
  )[[3]]
}
