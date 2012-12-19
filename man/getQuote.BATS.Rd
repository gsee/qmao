\name{getQuote.BATS}
\alias{depth}
\alias{getQuote.bats}
\alias{getQuote.BATS}
\alias{ladder}
\alias{trades}
\title{getQuote from BATS}
\usage{
  getQuote.BATS(Symbols,
    what = c("bats", "bbo", "ladder", "depth", "trades"),
    exch = c("bzx", "byx", "opt"))

  getQuote.bats(Symbols,
    what = c("bats", "bbo", "ladder", "depth", "trades"),
    exch = c("bzx", "byx", "opt"))

  ladder(x)

  trades(x)

  depth(x, ...)
}
\arguments{
  \item{Symbols}{a vector of ticker symbols.  If length is
  greater than 1, a list will be returned unless
  \code{what="bbo"} in which case a multi-row
  \code{data.frame} will be returned.}

  \item{what}{character. What to return.  See details}

  \item{exch}{character. "bzx" or "byx".  ("opt" is
  completely untested and most likely does not work)}

  \item{x}{a bats object}

  \item{...}{pass through arguments}
}
\value{
  market data.  The type of object returned depends on the
  value of \code{what}.  See Details.  If \code{Symbols} is
  of length greater than 1 and type is not "bbo", a list
  will be returned.
}
\description{
  \code{getQuote.BATS} is a
  \code{\link[quantmod]{getQuote}} "method" for BATS,
  although it may be called directly.
}
\details{
  This downloads data from BATS for a given stock and
  exchange and parses it into a list using
  \code{\link[RJSONIO]{fromJSON}}.  By default
  (\code{what="bats"}), this list is given a class of
  \code{bats} and returned. Currently, the other values for
  \code{what} just specify how to reformat/subset the data.

  With \code{what="bbo"}, a \code{data.frame} will be
  returned with a row for each of the \code{Symbols} and
  columns "BidSize", "BidPrice", "AskPrice", "AskSize",
  "Last", "LastSize" (any of those columns that are all NA
  are omitted; e.g. when the market is closed the returned
  \code{data.frame} will likely only have the "TradeTime",
  "Last" and "LastSize" columns.

  If \code{what="ladder"}, an object of class \code{ladder}
  is returned.  It will be a \code{matrix} at its core with
  3 columns: BidQty, Price, AskQty. The Price column is
  from highest to lowest. There should only be a BidQty or
  an AskQty at a given price, and the other will be NA
  (otherwise, the market would be crossed).  An object of
  class \code{ladder} will also have the following
  attributes: "timestamp", "company", "volume",
  "last.price", and "last.qty"

  When called with \code{what="depth"}, an object of class
  \code{depth} will be returned which is very similar to a
  \code{ladder} object except it has 4 columns and
  (usually) no NAs.  Instead of stacking the prices on top
  of each other, they are side-by-side.  A \code{depth}
  object is a \code{matrix} at its core and has 4 columns:
  "BidQty", "BidPrice", "AskPrice", "AskQty". Under usual
  market conditions it will have 5 rows.  It also has all
  the attributes that a \code{ladder} object has.

  If \code{what="trades"}, an xts object is returned that
  has the "Qty" and "Price" of the last 10 trades.

  There are \code{print} methods for \code{bats},
  \code{ladder}, and \code{depth} that make use of
  \code{pander::pandoc.table}

  There are also \code{plot} methods for \code{bats},
  \code{ladder}, and \code{depth}.  These make it
  incredibly easy to make a nice looking shiny app.
}
\note{
  "opt" exch has NOT been TESTED AT ALL
}
\examples{
\dontrun{ # can't run the examples because they require internet access
## These should be run while the U.S. stock market is open
getQuote("SPY", src="BATS")
getQuote("SPY", src="BATS", exch="byx")

getQuote("SPY", src="BATS", what="bbo")
getQuote("SPY", src="BATS", what="ladder")
print(getQuote("SPY", src="BATS", what="ladder"), header=FALSE)
getQuote("SPY", src="BATS", what="depth")
getQuote("SPY", src="BATS", what="trades")

plot(getQuote("SPY", src="BATS"))
plot(getQuote("SPY", src="BATS", what="ladder"))
plot(getQuote("SPY", src="BATS", what="depth"))
}
}
\author{
  Garrett See
}
\references{
  \url{http://www.batstrading.com/market_data/}
}
\seealso{
  \code{\link[quantmod]{getQuote}}
}

