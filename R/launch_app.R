#' Launch Sample Size Calculator Shiny App
#'
#' @export
launch_app <- function() {
  shiny::runApp(system.file("shiny", package = "clinicalsize"))
}
