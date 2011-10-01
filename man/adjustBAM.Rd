\name{adjustBAM}
\alias{adjustBAM}
\title{adjust prices}
\usage{
  adjustBAM(x)
}
\arguments{
  \item{x}{Either an xts OHLC or BAM object}
}
\value{
  an object of the same class with prices adjusted to be
  consistent with Adjusted column
}
\description{
  Adjust the prices to be consistent with Adjusted column
}
\details{
  Adjusts prices in Bid, Ask, Open, High, Low, Close, to be
  consistent with Adjusted column.  The adjusted column is
  assumed to have been calculated using simple
  addition/subtraction of cash flows. This is not for use
  on yahoo data. Use adjustOHLC for that.
}
\examples{
\dontrun{
##Do not acutally use on yahoo data
getSymbols('SPY')
adjustBAM(SPY)
}
}
\author{
  Garrett See
}
\seealso{
  adjustOHLC
}

