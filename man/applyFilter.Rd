\name{applyFilter}
\alias{applyFilter}
\title{Filter out bad prices}
\usage{
  applyFilter(BAM, filter = 1e-04)
}
\arguments{
  \item{BAM}{An xts object that has Bid, Ask, and}

  \item{filter}{a very small number that is the percentage
  of data to delete}
}
\value{
  Same as BAM only with fewer rows
}
\description{
  Deletes data where the Bid-Ask spread is extremely wide,
  which is probably a bad print
}
\details{
  If you want to delete rows where the bid ask spread is
  wider than it is 98 pct of the time, use filter=0.02
}
\note{
  If applyFilter is called with a filter value of 0, it may
  still filter the row with the widest Bid-Ask spread.

  This is intended to clean data with extremely low bid
  prices, or extremely high ask prices, but currently does
  not filter out rows with high bid prices or low ask
  prices.
}
\examples{
\dontrun{
#Create psuedo-BAM data
getSymbols('SPY')
SPY <- cbind(Lo(SPY),Hi(SPY),Cl(SPY))
colnames(SPY) <- paste("SPY",c('Bid.Price','Ask.Price','Mid.Price'),sep='.')
#make some bad prints
SPY[length(SPY[,1])-4,1] <- 0
SPY[length(SPY[,1])-3,2] <- 10000
SPY[length(SPY[,1])-1,1] <- 0
tail(SPY) #look at the raw data
SPY <- applyFilter(SPY, filter=1e-02)
tail(SPY)
}
#
}
\author{
  Garrett See
}
\seealso{
  quantile
}

