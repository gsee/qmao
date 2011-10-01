\name{makeTestData}
\alias{makeTestData}
\title{Split data to create in-sample and out-of-sample data sets}
\usage{
  makeTestData(x, ho.pct = 0.2)
}
\arguments{
  \item{x}{xts data object}

  \item{ho.pct}{percent of data to use for out-of-sample}
}
\value{
  \item{inSample}{first data set; used to create model}
  \item{outOfSample}{second data set for confirming results
  found using 1st dataset}
}
\description{
  #Give it an xts object and hold out pct, and it gives
  list with #first (1-\code{ho.pct}) percent of data, and
  last \code{ho.pct} of data
}
\examples{
\dontrun{
getSymbols('SPY')
splitdata <- makeTestData(SPY)
tail(splitdata[[1]])
head(splitdata[[2]])
}
}
\author{
  Garrett See
}

