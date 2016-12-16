#' trmsimUI
#'
#' Runs a shiny application in a web browser that enables to interactively
#' experiment with TRMs and their effect in the observed gene-wise correlations.
#'
#' @return NULL
#' @export
#'
trmsimUI <- function() {
  runApp(system.file("shiny", "trmsim", "app.R", package = "trmsim"))
}