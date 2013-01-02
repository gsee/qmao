#' getQuote from Google
#' 
#' \code{getQuote.google} is a \code{\link[quantmod]{getQuote}} "method" for
#' Google, although it may be called directly.
#'
#' This pulls real-time market data for stocks with tickers specified by 
#' \code{Symbols}.  Although this is not a documented API, it is used by the
#' Google finance website.  The data should only be used for personal use.
#'
#' @param Symbols character vector of ticker symbols, or comma or semi-colon 
#'   separated string
#' @param \dots pass through arguments; not currently in use
#' @return a data.frame with rownames corresponding to the ticker symbols, and
#'   having the following columns: "TradeTime", "Last", "Change", "PctChg",
#'   "Exchange", "GoogleID"
#' @author Dirk Eddelbuettel, Garrett See
#' @references http://digitalpbk.com/stock/google-finance-get-stock-quote-realtime
#' @seealso \code{link[quantmod]{getQuote}}, \code{link{getQuote.BATS}}
#' @examples
#' \dontrun{
#' getQuote("SPY", src="google")
#' getQuote(c("GS", "DE"), src="google")   # vector of Symbols
#' getQuote("EDD;SEE", src="google")       #semi-colon delimited
#' getQuote("GS,SEE,DE,EDD", src="google") #comma delimited
#' 
#' getQuote.google("GOOG") # Can call directly without using quantmod::getQuote
#' }
#' @export
getQuote.google <- function(Symbols, ...) {
  syms <- paste(unlist(strsplit(Symbols, ",|;")), collapse=",")
  base.url <- "http://finance.google.com/finance/info?client=ig&q="
  dat <- do.call(rbind, 
                 fromJSON(gsub("^// ", "", 
                          paste(readLines(paste(base.url, syms, sep="")), 
                                collapse=""))))
  ## getQuote.yahoo has these columns by default:
  ## Trade Time, Last, Change, % Change, Open, High, Low, Volume
  data.frame(TradeTime=strptime(dat[, "lt"], format="%b %d, %I:%M%p", tz="America/New_York"),
             Last=as.numeric(dat[, "l_cur"]),
             Change=as.numeric(dat[, "c"]),
             PctChg=as.numeric(dat[, "cp"]),
             Exchange=dat[, "e"],
             GoogleID=dat[, "id"],
             row.names=dat[, "t"],
             stringsAsFactors=FALSE)
  ## I don't know what these columns from Google's JSONP are: "s", "ccol"           
  ## In addition to those, I did not include:
  ##   "t" (it becomes row.names)
  ##   "l" (I think it's the same as l_cur)
  ##   "ltt" (Same as "lt", but without Date)
}

#as.data.frame(do.call(rbind, fromJSON(gsub("^// ", "", paste(readLines("http://finance.google.com/finance/info?client=ig&q=NASDAQ:GOOG,NASDAQ:YHOO"), collapse="")))), stringsAsFactors=FALSE)
#do.call(rbind, fromJSON(gsub("^// ", "", paste(readLines("http://finance.google.com/finance/info?client=ig&q=EEM,SCHE,AAPL"), collapse=""))))
#do.call(rbind, fromJSON(gsub("^// ", "", paste(readLines("http://finance.google.com/finance/info?client=ig&q=NYSE:IBM"), collapse=""))))

# XML ----
# TODO: implement XML version.  The only apparent advantage to the XML is that it 
# gives Open, High, and Low
#sapply(getNodeSet(xmlParse("http://www.google.com/ig/api?stock=SPY"), "//finance/*"), function(el) xmlGetAttr(el, "data"))
## The following does not work; not sure how to get quotes for multiple stocks in a single call.
#sapply(getNodeSet(xmlParse("http://www.google.com/ig/api?stock=SPY,IBM"), "//finance/*"), function(el) xmlGetAttr(el, "data"))



