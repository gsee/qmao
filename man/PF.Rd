\name{makePriceFrame}
\alias{makePriceFrame}
\alias{makeReturnFrame}
\alias{PF}
\alias{RF}
\title{Merge Adjusted prices (or returns of adjusted prices) of several symbols}
\usage{
  makePriceFrame(Symbols, from = NULL, to = NULL,
    prefer = NULL, notional = TRUE, na.omit = TRUE,
    subset = NULL, env = .GlobalEnv, silent)

  PF(Symbols, from = NULL, to = NULL, prefer = NULL,
    notional = TRUE, na.omit = TRUE, subset = NULL,
    env = .GlobalEnv, silent)

  makeReturnFrame(Symbols, ..., from = NULL, to = NULL,
    prefer = NULL, notional = TRUE, na.omit = TRUE,
    subset = NULL, env = .GlobalEnv, silent)

  RF(Symbols, ..., from = NULL, to = NULL, prefer = NULL,
    notional = TRUE, na.omit = TRUE, subset = NULL,
    env = .GlobalEnv, silent)
}
\arguments{
  \item{Symbols}{character vector of names of xts objects}

  \item{from}{include no data before this date/timestamp}

  \item{to}{include data through this date/timestamp}

  \item{prefer}{column to use. If NULL, the first of the
  following columns that is found will be used:
  \sQuote{Adjusted}, \sQuote{Close}, \sQuote{Mid}}

  \item{notional}{Should notional values be returned?
  Default is \code{TRUE} see Details}

  \item{na.omit}{Should \code{NA} values be removed?
  Default is \code{TRUE}}

  \item{subset}{xts style subsetting argument. (e.g.
  "T08:30/T15:00" or "last 10 days") Default is
  \code{NULL}}

  \item{env}{environment in which to look for xts objects}

  \item{silent}{silence warnings? (Currently, the only
  warning message is \dQuote{Instrument not found, using
  contract multiplier of 1})}

  \item{\dots}{arguments to be passed to \code{ROC}.  These
  can be \sQuote{n}, \sQuote{type}, \sQuote{na.pad},}
}
\value{
  xts object with same number of columns as length of
  \code{Symbols}, 1 for each symbol's [adjusted] prices
  (returns). The object will have a "prefer" \code{attr}
  indicating which column was used to build it.
}
\description{
  Given the names of symbols, merge together Adjusted
  columns of their xts objects
}
\details{
  \code{PF} and \code{RF} are aliases.
  \code{makePriceFrame} merges the \code{prefer} columns of
  several symbols if \code{prefer} is given.  Otherwise, it
  will use the adjusted column is if it exists, or close,
  mid, or price if it does not. (See \code{\link{estAd}}).

  If \code{notional} is \code{TRUE} and any \code{Symbols}
  are the names of
  \code{\link[FinancialInstrument]{instrument}}s, prices
  will be multiplied by their multipliers unless they have
  a \code{"notional"} \code{attr} that has a value of
  \code{TRUE} (which indicates that they have already been
  notionalized.  If the object has already been
  notionalized, and \code{notional} is \code{TRUE}, it will
  be used as is.  However, if it has already been
  notionalized (i.e. \code{isTRUE(attr(obj, 'notional'))})
  and the \code{notional} argument is \code{FALSE}, price
  will be divided by their multipliers to "denotionalize"
  them.

  \code{makeReturnFrame} first calls \code{makePriceFrame},
  then calculates returns on those price series

  The \code{from} and \code{to} arguments are intended to
  be used to indicate the starting and ending date, whereas
  the \code{subset} argument is intended to be used to
  subset intraday data by time of day.

  Since this function merges together the same
  \code{prefer} column from each Symbol, the column names
  can be simplified.  The \code{colnames} will simply be
  set to be the same as \code{Symbols}.  A "prefer"
  \code{attr} will be added to the returned object so that
  you can see (using \code{str}, or \code{attr(x,
  "prefer")}) what column was used to build the PriceFrame.
}
\note{
  \code{makeReturnFrame} can be useful before calling
  functions like charts.PerformanceSummary from the
  PortfolioAnalytics package.
}
\examples{
\dontrun{
getSymbols(c('SPY','DIA','QQQ'))
pf <- makePriceFrame(c('SPY','DIA','QQQ'))
head(pf)
rf <- makeReturnFrame(c('SPY','DIA','QQQ'))
head(rf)
## See what column was used to make pf
attr(pf, "prefer")
}
}
\author{
  Garrett See
}
\seealso{
  \code{\link{estAd}}, \code{\link[quantmod]{getPrice}},
  \code{\link[base]{attr}}, \code{\link{gsa}},
  \code{\link{alignSymbols}}, merge, cbind
}

