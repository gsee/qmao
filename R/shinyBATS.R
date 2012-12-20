#' shiny BATS quotes
#' 
#' Start a shiny application that shows the top five levels of the BATS order 
#' book for a stock.
#' 
#' This shiny app simply calls \code{\link{getQuote.BATS}} and plots the result.
#' It requests new data every second, but the data from BATS only updates every
#' five seconds.
#' 
#' The code for this shiny app can be found in the shinyBATS directory at the 
#' top level of this package.
#' @param port The TCP port that the application should listen on.  Defaults to
#'   port 8100.
#' @param launch.browser If \code{TRUE}, the system's default web browser will
#'   be launched automatically after the app is started.  Defaults to 
#'   \code{TRUE} in interactive sessions only.
#' @param ... arguments to \code{\link[shiny]{runApp}}
#' @author Garrett See
#' @note documentation for \code{shiny} arguments copied from 
#'   \code{?shiny::runApp}
#' @seealso \code{\link{getQuote.BATS}}, \code{\link{getQuote}}, 
#'   \code{\link[shiny]{runApp}}
#' @export
shinyBATS <- function(port=8100L, 
                      launch.browser=getOption("shiny.launch.browser", 
                                               interactive())) {
    runApp(system.file('shinyBATS', package='qmao'), port=port, 
           launch.browser=launch.browser)
}
