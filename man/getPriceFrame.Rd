\name{getPriceFrame}
\alias{getPriceFrame}
\title{
get prices for 2 instruments
}
\description{
merges together Adjusted columns of 2 xts objects
}
\usage{
getPriceFrame(symbols)
}
\arguments{
  \item{symbols}{
vector of character strings 
}
}
\details{
Merges a price columns of 2 symbols where the column is adjusted if it exists,
or one of close, mid, price if it does not. 
}
\value{
xts object with 2 columns: 1 for each symbol's [adjusted] prices
}
\references{

}
\author{
Garrett See
}
\seealso{
estAd, getPrice, merge
}
\examples{
getSymbols(c('SPY','DIA'))
pf <- getPriceFrame(c('SPY','DIA'))
head(pf)
}
