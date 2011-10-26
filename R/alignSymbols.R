#' Delete rows that not all symbols have in common
#' 
#' Deletes rows that not all symbols have in common
#' 
#' alignSymbols will get symbols from the specified environment (.GlobalEnv by
#' default), merge them together, and delete all rows that have any NAs.  Then
#' it will \sQuote{unmerge} and assign the cleaned data back to the xts objects
#' named by \code{symbols}.
#' 
#' @param symbols character vector of symbol names which correspond to xts
#' objects
#' @param env The environment that holds the xts objects
#' @return used for its side effect.
#' @author Garrett See
#' @seealso \code{\link{gsa}}, \code{\link{PF}}, merge, merge.xts, cbind, cbind.xts, assign
#' @examples
#' 
#' \dontrun{
#' symbols <- c("SPY","DIA","CORN")
#' getSymbols(symbols)
#' length(Cl(SPY));length(Cl(DIA));length(Cl(CORN))
#' alignSymbols(symbols)
#' length(Cl(SPY));length(Cl(DIA));length(Cl(CORN))
#' }
#' @export
alignSymbols <-
function(symbols, env=.GlobalEnv) {
	if (length(symbols) < 2) 
		stop("Must provide at least 2 symbols")
	if (any(!is.character(symbols))) 
		stop("Symbols must be vector of character strings.")
	ff <- try(get(symbols[1],pos=env))
	if (inherits(ff, 'try-error') || (!inherits(ff,'xts') && !inherits(ff,'zoo'))) stop('You must create an xts object for each symbol first.')
	ncols <- ncol(ff)	#compare all symbols ncols to 1st one; make sure they're the same
    symout <- symbols[1]	
    for (sym in symbols[-1]) {
		tmp.sym <- try(get(sym,pos=env),silent=TRUE)
        if (!inherits(tmp.sym,'try-error') && (inherits(tmp.sym,'xts') || inherits(tmp.sym,'zoo'))) {
		    if (ncol(tmp.sym) != ncols) stop("All symbols must have the same number of columns.")
		    ff <- merge(ff,tmp.sym,all=FALSE)
		    ff <- na.omit(ff)
            symout <- c(symout, sym)
        }
	}
	for (sym in symout) {
        asym <- strsplit(sym,"_")[[1]][1] #only use root of futures or options in col names
		assign(sym,ff[,grep(asym,colnames(ff))],pos=env)
	}
	symout
}

