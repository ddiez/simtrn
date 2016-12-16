#' simtrnUI
#'
#' Runs a shiny application in a web browser that enables to interactively
#' experiment with TRMs and their effect in the observed gene-wise correlations.
#'
#' @return NULL
#' @export
#'
simtrnUI <- function() {
  runApp(system.file("shiny", "simtrn", "app.R", package = "simtrn"))
}