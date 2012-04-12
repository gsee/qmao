#' Get historical Earnings Per Share of a stock
#'
#' This will use \code{\link[quantmod]{getFinancials}} to download 
#' the financial statements for \code{Symbol}.  Then it will extract
#' only the \dQuote{Diluted Normalized EPS} row from the Income 
#' Statement and convert it to an xts object. 
#' @param Symbol A single valid google symbol as a string. (may be updated in the future to support vector and semi-colon delimited)
#' @param freq Quarterly or Annual (but, only the first letter is used, so "Q" and "A" work also)
#' @param src Only 'google' is currently supported.
#' @return an xts object containing historical Diluted Normalized EPS according to google.
#' @seealso \code{\link{getEarnings}} for historic earnings estimates 
#'   and actual earnings from earnings.com.
#' @examples
#' \dontrun{
#' getEPS("GOOG") # Quarterly by default
#' getEPS("GOOG","Annual")
#'
#' # get a single xts object with several stocks' EPS
#' do.call(cbind, lapply(c("GOOG","MSFT","IBM"), getEPS))
#' }
#' @export
getEPS <- function(Symbol, freq=c("Q","A"), src='google') {
    freq <- toupper(substr(freq[1],1,1))
    f <- getFinancials(Symbol, src=src, auto.assign=FALSE)
    IS <- f$IS[[freq]]
    e <- IS[nrow(IS),]
    out <- as.xts(t(t(e)))   
    colnames(out) <- paste(Symbol,freq,'EPS',sep=".")
    out
}

