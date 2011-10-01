#' Close to Open return
#' 
#' Calculates returns from previous days' closes to current days' opens
#' 
#' @param x an xts object with Close and Open columns
#' @return an xts object of returns
#' @author Garrett See
#' @seealso OpOp, ClCl, OpCl
#' @examples
#' \dontrun{
#' getSymbols("SPY")
#' ClOp(SPY)
#' }
#' @export
ClOp <-
function(x)
{
	xx <- Delt(Lag(Cl(x)),Op(x))
	colnames(xx) <- paste("ClOp", deparse(substitute(x)), sep='.')
	xx
}

