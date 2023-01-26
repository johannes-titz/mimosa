#' Starts the mimosa shiny app
#' @param host default is from getOption shiny.host 
#' @param port default is from getOption shiny.port
#' @import shiny
#' @export
run_app <- function(host = getOption("shiny.host"),
                    port = getOption("shiny.port")) {
  shinyApp(
    ui = myui(),
    server = server,
    options = list(host = host, port = port)
  )
}