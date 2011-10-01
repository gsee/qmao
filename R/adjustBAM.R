#' adjust prices
#' 
#' Adjust the prices to be consistent with Adjusted column
#' 
#' Adjusts prices in Bid, Ask, Open, High, Low, Close, to be consistent with
#' Adjusted column.  The adjusted column is assumed to have been calculated
#' using simple addition/subtraction of cash flows. This is not for use on
#' yahoo data. Use adjustOHLC for that.
#' 
#' @param x Either an xts OHLC or BAM object
#' @return an object of the same class with prices adjusted to be consistent
#' with Adjusted column
#' @author Garrett See
#' @seealso adjustOHLC
#' @examples
#' 
#' \dontrun{
#' ##Do not acutally use on yahoo data
#' getSymbols('SPY')
#' adjustBAM(SPY)
#' }
#' @export
adjustBAM <-
function(x) { 
	symbol.name <- deparse(substitute(x))
	if (!has.Ad(x))
		stop("no Adjusted column in 'x'")
	if (!is.BBO(x)) { #it's probably OHLC data
		if (is.OHLC(x)) {
			adjustment <- Ad(x)-Cl(x)
			Adjusted <- Cl(x) + adjustment
			structure(cbind((adjustment + Op(x)), (adjustment + Hi(x)), adjustment + Lo(x), Adjusted,
				if (has.Vo(x))
					Vo(x)
				else NULL,
				if (has.Ad(x))
					Ad(x)
				else NULL), .Dimnames = list(NULL, colnames(x)))	
		} else stop('Unrecognized column names. Missing Bid,Ask,High,Low,or Close')
	} else { #Not OHLC data; most likely BBO
		ifelse(has.Mid(x),adjustment <- Ad(x)-Mi(x), adjustment <- Ad(x)-getPrice(x))
		ifelse(has.Mid(x),Adjusted <- Mi(x) + adjustment,Adjusted <- getPrice(x)+adjustment)
		structure(cbind((adjustment  + Bi(x)), (adjustment + As(x)), Adjusted, 
				if (has.Chg(x))
					Ch(x)
				else NULL, 
				if (has.Ad(x))
					Ad(x)
				else NULL,
				if (has.Vo(x))
					Vo(x)
				else NULL), .Dimnames = list(NULL, colnames(x)))
	}
}

