\name{addVLine}
\alias{addVLine}
\title{
Add vertical line to a quantmod chart
}
\description{
Adds a vertical line to the current quantmod chart
}
\usage{
addVLine(dtlist)
}

\arguments{
  \item{dtlist}{
Plots a vertical line at given index
}
}
\details{
}
\value{
used for its side effect.
}
\references{
}
\author{
Garrett See
}
\note{
}


\seealso{
chartSeries, addTA
}
\examples{
getSymbols('SPY',adjust=TRUE)
chartSeries(SPY)
addVLine(index(SPY['2011-05-20::2011-05-25']))
}
