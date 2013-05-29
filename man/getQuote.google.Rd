\name{getQuote.google}
\alias{getQuote.google}
\title{getQuote from Google}
\usage{
  getQuote.google(Symbols, ...)
}
\arguments{
  \item{Symbols}{character vector of ticker symbols, or a
  comma or semi-colon separated string}

  \item{\dots}{not currently in use}
}
\value{
  a data.frame with rownames corresponding to the ticker
  symbols, and having the following columns: "TradeTime",
  "Last", "Change", "PctChg", "Exchange", "GoogleID"
}
\description{
  \code{getQuote.google} is a
  \code{\link[quantmod]{getQuote}} "method" for Google,
  although it may be called directly.
}
\details{
  This pulls real-time market data for stocks with tickers
  specified by \code{Symbols}.  Although this is not a
  documented API, it is used by the Google finance website.
  The data should only be used for personal use.

  Only 100 Symbols may be requested from google at a time.
  If \code{getQuote.google} is called with more than 100
  Symbols, blocks of 100 Symbol calls will be made and the
  results will be put in a single \code{data.frame}.  This
  part of the code is mostly copied from Jeff Ryan's
  \code{\link[quantmod]{getQuote}}.yahoo
}
\examples{
\dontrun{
getQuote("SPY", src="google")
getQuote("TYO:8316", src="google")      # non-U.S.
getQuote(c("GS", "DE"), src="google")   # vector of Symbols
getQuote("EDD;SEE", src="google")       #semi-colon delimited
getQuote("GS,SEE,DE,EDD", src="google") #comma delimited

getQuote.google("GOOG") # Can call directly without using quantmod::getQuote
}
}
\author{
  Dirk Eddelbuettel, Jeff Ryan, Garrett See
}
\references{
  \url{http://digitalpbk.com/stock/google-finance-get-stock-quote-realtime}
}
\seealso{
  \code{\link[quantmod]{getQuote}},
  \code{\link{getQuote.BATS}}
}

