\name{makeTestData}
\alias{makeTestData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Split data to create in-sample and out-of-sample data sets
}
\description{
#Give it an xts object and hold out pct, and it gives list with 
#first (1-ho.pct) percent of data, and last ho.pct of data
}
\usage{
makeTestData(x, ho.pct = 0.2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
xts data object
}
  \item{ho.pct}{
percent of data to use for out-of-sample
}
}
\details{

}
\value{
  \item{inSample }{first data set; used to create model}
  \item{outOfSample }{second data set for confirming results found using 1st dataset}
}
\author{
Garrett See
}


\examples{
getSymbols('SPY')
splitdata <- makeTestData(SPY)
tail(splitdata[[1]])
head(splitdata[[2]])
}
