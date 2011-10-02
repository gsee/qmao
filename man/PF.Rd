\name{makePriceFrame}
\alias{makePriceFrame}
\alias{makeReturnFrame}
\alias{PF}
\alias{RF}
\title{Merge Adjusted prices (or returns of adjusted prices) of several symbols}
\usage{
  makePriceFrame(symbols, from = NULL, to = NULL, prefer =
  NULL, notional = TRUE, na.omit = TRUE, subset = NULL, env
  = .GlobalEnv, silent = FALSE)

  PF(symbols, from = NULL, to = NULL, prefer = NULL,
  notional = TRUE, na.omit = TRUE, subset = NULL, env =
  .GlobalEnv, silent = FALSE)

  makeReturnFrame(symbols, from = NULL, to = NULL, prefer =
  NULL, notional = TRUE, na.omit = TRUE, subset = NULL, env
  = .GlobalEnv, silent = FALSE, ...)

  RF(symbols, from = NULL, to = NULL, prefer = NULL,
  notional = TRUE, na.omit = TRUE, subset = NULL, env =
  .GlobalEnv, silent = FALSE, ...)
}
\arguments{
  \item{symbols}{vector of character strings}

  \item{from}{include no data before this date/timestamp}

  \item{to}{include data through this date/timestamp}

  \item{prefer}{column to use. If NULL, the first of the
  following columns that is found will be used:
  \sQuote{Adjusted}, \sQuote{Close}, \sQuote{Mid}}

  \item{notional}{Should the prices will be multiplied by
  their multipliers, to get notional values? Default is
  \code{TRUE}}

  \item{na.omit}{Should \code{NA} values be removed?
  Default is \code{TRUE}}

  \item{subset}{xts style subsetting argument. (e.g.
  "T08:30/T15:00") Default is \code{NULL}}

  \item{env}{environment in which to look for xts objects}

  \item{silent}{silence warnings? (Currently, the only
  warning message is \dQuote{Instrument not found, using
  contract multiplier of 1})}

  \item{\dots}{arguments to be passed to \code{ROC}.  These
  can be \sQuote{n}, \sQuote{type}, \sQuote{na.pad},}
}
\value{
  xts object with same number of columns as length of
  \code{symbols}: 1 for each symbol's [adjusted] prices
  (returns)
}
\description{
  Given the names of symbols, merge together Adjusted
  columns of their xts objects
}
\details{
  \code{PF} and \code{RF} are aliases.
  \code{makePriceFrame} merges price columns of several
  symbols where the column is adjusted if it exists, or
  close, mid, or price if it does not.
  \code{makeReturnFrame} first calls \code{makePriceFrame},
  then calculates returns on those price series

  The \code{from} and \code{to} arguments are intended to
  be used to indicate the starting and ending date, whereas
  the \code{subset} argument is intended to be used to
  subset intraday data by time of day.
}
\note{
  \code{makeReturnFrame} can be useful before calling
  charts.PerformanceSummary from the PortfolioAnalytics
  package.
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
\author{
  Garrett See
}
\seealso{
  estAd, getPrice, merge, cbind
}

