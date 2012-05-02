\name{AddCumDiv}
\alias{AddCumDiv}
\title{Add dividends back to price series}
\usage{
  AddCumDiv(x, name, env = .GlobalEnv)
}
\arguments{
  \item{x}{xts or character name of xts object}

  \item{name}{name of stock to use when getting dividends}

  \item{env}{environment in which to find \code{x}}
}
\value{
  xts object with same dims as \code{x} that is the
  original \code{x} with cumulative dividends added.  An
  \code{attr} called \dQuote{cumdiv} that contains an xts
  of the cumulative dividends will be added to the returned
  object
}
\description{
  At every timestamp, add to the prices the cumulative
  amount of dividends receieved since the beginning of the
  dataset.
}
\details{
  After a series has been adjusted, an "adj" attr of TRUE
  is added to the object.  If the object already has an
  "adj" attr, the original object \code{x} will be returned
  and no adjustment will be made.

  cumulative dividends will only be added to columns with
  names that include one of the following terms: Open,
  High, Low, Close, Bid.Price, Ask.Price, Trade.Price,
  Mid.Price.

  TODO: make a function that does the opposite of this one.
}
\examples{
\dontrun{
## You must setDefaults on getSymbols.FI (at least for the "dir" argument)
## for this to work
getSymbols("SPY", src='FI', from='2011-09-01', to='2011-12-31')
SPY.tp <- AddCumDiv(SPY)
tail(attr(SPY.tp, "cumdiv"))
cbind(tail(estAd(SPY)), estAd(tail(SPY.tp)))
}
}
\author{
  Garrett See
}

