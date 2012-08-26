\name{addVLine}
\alias{addVLine}
\title{Add vertical line to a quantmod chart}
\usage{
  addVLine(dtlist, on = -1, col = "blue", ...)
}
\arguments{
  \item{dtlist}{Plots a vertical line at given index.
  Alternatively, this can be one of the following strings:
  \dQuote{secs}, \dQuote{seconds}, \dQuote{mins},
  \dQuote{minutes}, \dQuote{hours}, \dQuote{days},
  \dQuote{weeks}, \dQuote{months}, \dQuote{years}}

  \item{on}{panel to add vertical line on}

  \item{col}{color of the line}

  \item{...}{parameters to pass to
  \code{\link[quantmod]{addTA}}.  (e.g. `border=NA`)}
}
\value{
  used for its side effect.
}
\description{
  Adds a vertical line to the current quantmod chart
}
\details{
  If \code{dtlist} is a character string, a vertical line
  will be drawn at the end of the periods specified by
  \code{dtlist} which will be used as the \code{on}
  argument in a call to \code{\link[xts]{endpoints}}.  (Not
  to be confused with the \code{on} argument of this
  function which is used to indicate which panel to plot
  the vertical line(s) on.) Therefore, if \code{dtlist} is
  character, it must be a valid value for the \code{on}
  argument of the \code{\link{endpoints}} function which
  (from \code{\link{endpoints}} help) include: \dQuote{us}
  (microseconds), \dQuote{microseconds}, \dQuote{ms}
  (milliseconds), \dQuote{milliseconds}, \dQuote{secs}
  (seconds), \dQuote{seconds}, \dQuote{mins} (minutes),
  \dQuote{minutes}, \dQuote{hours}, \dQuote{days},
  \dQuote{weeks}, \dQuote{months}, \dQuote{quarters}, and
  \dQuote{years}.
}
\note{
  Currently, this function is sort of a hack.  It does not
  update the \code{chob}.  So, if you use this function,
  then you will no longer be able to do things like
  \code{\link[quantmod]{zoomChart}}.  There are a few
  variations of this function in the works.
}
\examples{
\dontrun{
getSymbols('SPY', src='yahoo', from='2011-01-01', to='2012-08-01')
chartSeries(SPY)
addVLine('months')
addVLine(index(SPY['2011-05-20::2011-05-25']))
}
}
\references{
  \url{https://stat.ethz.ch/pipermail/r-sig-finance/2009q2/004018.html}
}
\seealso{
  chartSeries, addTA
}

