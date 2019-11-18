#' Starts the mimosa shiny app
#'
#' @import shiny
#' @export
run_app <- function() {
  shinyApp(
    ui = myui(),
    server = myserver
  )
}