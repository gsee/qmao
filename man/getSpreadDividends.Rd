\name{getSpreadDividends}
\alias{getSpreadDividends}
\title{Get Spread Dividends...}
\usage{
  getSpreadDividends(spread_id, from = "2007-01-01", to =
  Sys.Date(), auto.assign = TRUE, env = .GlobalEnv)
}
\arguments{
  \item{spread_id}{chr primary identifier for an already
  defined spread instrument}

  \item{from}{Date from which to get dividends}

  \item{to}{get dividends through this date}

  \item{auto.assign}{if TRUE (default), spread dividends
  will be stored in an environment}

  \item{env}{where to get data. If \code{auto.assign} is
  TRUE it is also where the spread dividends will be
  stored}
}
\value{
  if \code{auto.assign} is TRUE the output will be written
  in the env specified with a .div appended to the name,
  and only the name of the symbol where the dividends were
  stored will be returned.

  otherwise, an xts object containing dividends with
  negative values indicating payments in leiu of dividends.
}
\description{
  Get Spread Dividends
}
\details{
  Given a spread_id, this will look for dividend data for
  each of the constituent legs of the spread in the the env
  specified.  If dividend data are not in \code{env} a call
  will be made to \code{getSymbols} The memberratio (stored
  in the spread instrument) will be used to weight the
  dividends.  Negative values in \sQuote{memberratio}
  indicate short positions.  Dividends on short legs will
  be negative.
}
\examples{
\dontrun{
s <- define_stocks(c("SPY","DIA"))
spread('spydia', 'USD', members=s, memberratio=c(1,-1))
getSpreadDividends('spydia')
spydia.div
}
}

