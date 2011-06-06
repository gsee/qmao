\name{adjustBAM}
\alias{adjustBAM}
\title{adjust}
\description{
Adjust the prices to be consistent with Adjusted column
}
\usage{
adjustBAM(x)
}

\arguments{
  \item{x}{
Either an xts OHLC or BAM object
}
}
\details{
Adjusts prices in Bid, Ask, Open, High, Low, Close, to be consistent with Adjusted column.  
The adjusted column is assumed to have been calculated using simple addition/subtraction of cash flows.
This is not for use on yahoo data. Use adjustOHLC for that.
}
\value{
an object of the same class with prices adjusted to be consistent with Adjusted column
}
\references{

}
\author{
Garrett See
}
\note{

}
\seealso{
adjustOHLC
}
\examples{

##Do not acutally use on yahoo data
getSymbols('SPY')
adjustBAM(SPY)
}
