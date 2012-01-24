#' get column containing Price
#' 
#' \code{estAd} returns values in Adjusted
#' column if it exists, or it's best guess if it doesn't
#' 
#' If \code{prefer} is not NULL, \code{estAd} will simply call
#' \code{\link{getPrice}} with \code{symbol} and \code{prefer}.  Otherwise,
#' \code{estAd} will look for "adjusted", "close", "mid", "price", "trade", in
#' that order.  If and only if those columns cannot be found, \code{symbol}
#' will be passed in a call to \code{getPrice}
#' 
#' @aliases estAd
#' @param x xts object
#' @param symbol character string
#' @param prefer for \code{estAd} what column to get if none of the defaults
#' can be found.
#' @return xts object with only one column of prices
#' @note case insensitive
#' @author Garrett See
#' @seealso \code{\link{getPrice}}, Op, Hi, Lo, \code{\link{Cl}}, Bi, As, 
#' \code{\link{Mi}}, Tr
#' @examples
#' 
#' \dontrun{
#' getSymbols('SPY')
#' estAd(SPY)
#' }
#' @export
#' @rdname estAd
estAd <-
function(x, symbol = NULL, prefer=NULL) {
	#get Adjusted price if available, else find alternative
    if (is.null(prefer)) {
	    if (has.Ad(x)) return(Ad(x))
	    else if (has.Cl(x)) return(Cl(x))
	    else if (has.Mid(x) || (has.Bid(x) && has.Ask(x))) return(Mi(x))
	    else getPrice(x, prefer=NULL) # looks for price, then trade
    } else getPrice(x,prefer=prefer)
}

