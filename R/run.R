#' Run the app
#'
#' @param port Port Number to use
#'
#' @import promises future xml2 data.table shiny utils
#' @export
run <- function(port = getOption("shiny.port")) {

  # We use promises, enable the 'multiprocess' plan
  plan(multiprocess)

  myapp <- shinyApp(ui = appui, server = appserver)	
  runApp(myapp, port=port)
}
