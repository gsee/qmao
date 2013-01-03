#' getQuote from Google
#' 
#' \code{getQuote.google} is a \code{\link[quantmod]{getQuote}} "method" for
#' Google, although it may be called directly.
#'
#' This pulls real-time market data for stocks with tickers specified by 
#' \code{Symbols}.  Although this is not a documented API, it is used by the
#' Google finance website.  The data should only be used for personal use.
#'
#' Only 100 Symbols may be requested from google at a time.  If 
#' \code{getQuote.google} is called with more than 100 Symbols, blocks of 100
#' Symbol calls will be made and the results will be put in a single 
#' \code{data.frame}.  This part of the code is mostly copied from Jeff Ryan's
#' \code{\link[quantmod]{getQuote}}.yahoo
#' 
#' @param Symbols character vector of ticker symbols, or a comma or semi-colon 
#'   separated string
#' @param \dots not currently in use
#' @return a data.frame with rownames corresponding to the ticker symbols, and
#'   having the following columns: "TradeTime", "Last", "Change", "PctChg",
#'   "Exchange", "GoogleID"
#' @author Dirk Eddelbuettel, Jeff Ryan, Garrett See
#' @references \url{http://digitalpbk.com/stock/google-finance-get-stock-quote-realtime}
#' @seealso \code{\link[quantmod]{getQuote}}, \code{\link{getQuote.BATS}}
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
  syms <- gsub(" ", "", unlist(strsplit(Symbols, ",|;")))
  sym.string <- paste(syms, collapse=",")
  length.of.symbols <- length(syms)
  base.url <- "http://finance.google.com/finance/info?client=ig&q="
  if (length.of.symbols > 100) {
    # google only returns up to 100 symbols per call; call getQuote.yahoo 
    # recursivly to handle each block of 100.  This code is mostly copied from
    # quantmod::getQuote.yahoo (c) Jeff Ryan
    all.symbols <- lapply(seq(1, length.of.symbols, 100),
                          function(x) na.omit(syms[x:(x + 99)]))
    df <- NULL
    cat("downloading set: ")
    for(i in 1:length(all.symbols)) {
      Sys.sleep(0.1)
      cat(i,", ")
      df <- rbind(df, getQuote.google(all.symbols[[i]]))
    }
    cat("...done\n")
    return(df)
  }
  # FIXME: check columns. Especially in the after-hours, some stocks do not have
  # as many columns as others. Specifically, these columns aren't always 
  # reported (most of these presumably refer to "electronic" after hours trading):
  # "el", "el_cur", "elt", "ec", "ecp", "eccol", "div", "yld"
  dat <- do.call(rbind, 
                 fromJSON(gsub("^// ", "", 
                          paste(readLines(paste(base.url, sym.string, sep="")), 
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

# all currently known column names:
# "id", "t", "e", "l", "l_cur", "s", "ltt", "lt", "c", "cp", 
# "ccol", "el", "el_cur", "elt", "ec", "ecp", "eccol", "div", "yld"


#as.data.frame(do.call(rbind, fromJSON(gsub("^// ", "", paste(readLines("http://finance.google.com/finance/info?client=ig&q=NASDAQ:GOOG,NASDAQ:YHOO"), collapse="")))), stringsAsFactors=FALSE)
#do.call(rbind, fromJSON(gsub("^// ", "", paste(readLines("http://finance.google.com/finance/info?client=ig&q=EEM,SCHE,AAPL"), collapse=""))))
#do.call(rbind, fromJSON(gsub("^// ", "", paste(readLines("http://finance.google.com/finance/info?client=ig&q=NYSE:IBM"), collapse=""))))

# XML ----
# TODO: implement XML version.  The only apparent advantage to the XML is that it 
# gives Open, High, and Low
#sapply(getNodeSet(xmlParse("http://www.google.com/ig/api?stock=SPY"), "//finance/*"), function(el) xmlGetAttr(el, "data"))
## The following does not work; not sure how to get quotes for multiple stocks in a single call.
#sapply(getNodeSet(xmlParse("http://www.google.com/ig/api?stock=SPY,IBM"), "//finance/*"), function(el) xmlGetAttr(el, "data"))



