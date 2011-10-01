#' Add vertical line to a quantmod chart
#' 
#' Adds a vertical line to the current quantmod chart
#' 
#' @param dtlist Plots a vertical line at given index
#' @param on panel to add vertical line on
#' @param col color of the line
#' @return used for its side effect.
#' @author Garrett See
#' @seealso chartSeries, addTA
#' @examples
#' \dontrun{
#' getSymbols('SPY',adjust=TRUE)
#' chartSeries(SPY)
#' addVLine(index(SPY['2011-05-20::2011-05-25']))
#' }
#' @export
addVLine <-
function(dtlist, on=1, col='blue') #add vertical line to a chart
 {
   plot(addTA(xts( rep(TRUE, NROW(dtlist)), dtlist), on=on, col=col))
 }

