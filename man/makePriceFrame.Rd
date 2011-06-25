\name{makePriceFrame}
\alias{makePriceFrame}
\alias{makeReturnFrame}
\alias{PF}
\alias{RF}
\title{
Merge Adjusted prices (or returns of adjusted prices) of several symbols
}
\description{
Given the names of symbols, merge together Adjusted columns of their xts objects
}
\usage{
makePriceFrame(symbols, from=NULL, to=NULL, prefer=NULL, env=.GlobalEnv)
makeReturnFrame(symbols, from=NULL, to=NULL, prefer=NULL, env = .GlobalEnv, ...)
}
\arguments{
  \item{symbols}{
vector of character strings 
}
  \item{from}{
include no data before this date/timestamp
}
  \item{to}{
include data through this date/timestamp
}
  \item{env}{
environment in which to look for xts objects
}
  \item{prefer}{
column to use. If NULL, the first of the following columns that is found will be used: Adjusted, Close, Mid
}
  \item{\dots}{
arguments to be passed to \code{ROC}.  These can be \sQuote{n}, \sQuote{type}, \sQuote{na.pad}, 
}
}
\details{
\code{PF} and \code{RF} are aliases.
\code{makePriceFrame} merges price columns of several symbols where the column is adjusted if it exists,
or close, mid, or price if it does not. 
\code{makeReturnFrame} first calls \code{makePriceFrame}, then calculates returns on those price series
}
\value{
xts object with same number of columns as length of \code{symbols}: 1 for each symbol's [adjusted] prices (returns)
}
\note{
\code{makeReturnFrame} can be useful before calling charts.PerformanceSummary from the PortfolioAnalytics package.
}
\author{
Garrett See
}
\seealso{
estAd, getPrice, merge, cbind
}
\examples{
\dontrun{
getSymbols(c('SPY','DIA','QQQ'))
pf <- makePriceFrame(c('SPY','DIA','QQQ'))
head(pf)
rf <- makeReturnFrame(c('SPY','DIA','QQQ'))
head(rf)
}
}
