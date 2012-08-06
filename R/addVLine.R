#' Add vertical line to a quantmod chart
#' 
#' Adds a vertical line to the current quantmod chart
#' 
#' If \code{dtlist} is a character string, a vertical line will be drawn at the
#' end of the periods specified by \code{dtlist} which will be used as the 
#' \code{on} argument in a call to \code{\link[xts]{endpoints}}.  (Not to be 
#' confused with the \code{on} argument of this function which is used to
#' indicate which panel to plot the vertical line(s) on.)
#' Therefore, if \code{dtlist} is character, it must be a valid value
#' for the \code{on} argument of the \code{\link{endpoints}} function which 
#' (from \code{\link{endpoints}} help) include: \dQuote{us} (microseconds),
#' \dQuote{microseconds}, \dQuote{ms} (milliseconds),
#' \dQuote{milliseconds}, \dQuote{secs} (seconds),
#' \dQuote{seconds}, \dQuote{mins} (minutes), \dQuote{minutes},
#' \dQuote{hours}, \dQuote{days}, \dQuote{weeks}, \dQuote{months}, 
#' \dQuote{quarters}, and \dQuote{years}.
#'
#' @param dtlist Plots a vertical line at given index. Alternatively, this can
#'   be one of the following strings: \dQuote{secs}, \dQuote{seconds}, 
#'   \dQuote{mins}, \dQuote{minutes}, \dQuote{hours}, \dQuote{days}, 
#'   \dQuote{weeks}, \dQuote{months}, \dQuote{years}
#' @param on panel to add vertical line on
#' @param col color of the line
#' @return used for its side effect.
#' @seealso chartSeries, addTA
#' @references 
#' \url{https://stat.ethz.ch/pipermail/r-sig-finance/2009q2/004018.html}
#' @examples
#' \dontrun{
#' getSymbols('SPY', src='yahoo', from='2011-01-01', to='2012-08-01')
#' chartSeries(SPY)
#' addVLine('months')
#' addVLine(index(SPY['2011-05-20::2011-05-25']))
#' }
#' @export
addVLine <-
function(dtlist, on=-1, col='blue') #add vertical line to a chart
{
  if (is.character(dtlist)) {
    lchob <- quantmod:::get.current.chob()
    #lchob <- get.current.chob()
    xdata <- lchob@xdata
    #xdata <- slot(quantmod:::get.chob()[[2]], "xdata")
    #xdata <- slot(get(".chob", pos=.GlobalEnv)[[2]], "xdata")
    dtlist <- index(xdata[endpoints(xdata, dtlist)])
  }
  plot(addTA(xts( rep(TRUE, NROW(dtlist)), dtlist), on=on, col=col))
}


