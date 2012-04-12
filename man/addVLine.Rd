\name{addVLine}
\alias{addVLine}
\title{Add vertical line to a quantmod chart}
\usage{
  addVLine(dtlist, on = 1, col = "blue")
}
\arguments{
  \item{dtlist}{Plots a vertical line at given index}

  \item{on}{panel to add vertical line on}

  \item{col}{color of the line}
}
\value{
  used for its side effect.
}
\description{
  Adds a vertical line to the current quantmod chart
}
\examples{
\dontrun{
getSymbols('SPY',adjust=TRUE)
chartSeries(SPY)
addVLine(index(SPY['2011-05-20::2011-05-25']))
}
}
\references{
  \url{https://stat.ethz.ch/pipermail/r-sig-finance/2009q2/004018.html}
}
\seealso{
  chartSeries, addTA
}

