#' Delete rows that not all Symbols have in common
#' 
#' Deletes rows that not all Symbols have in common
#' 
#' alignSymbols will get Symbols from the specified environment (.GlobalEnv by
#' default), merge them together, and delete all rows that have any NAs.  Then
#' it will \sQuote{unmerge} and assign the cleaned data back to the xts objects
#' named by \code{Symbols}.
#' 
#' @param Symbols character vector of symbol names which correspond to xts
#' objects
#' @param env The environment that holds the xts objects
#' @return used for its side effect.
#' @author Garrett See
#' @seealso \code{\link{gsa}}, \code{\link{PF}}, merge, merge.xts, cbind, 
#' cbind.xts, assign
#' @examples
#' 
#' \dontrun{
#' Symbols <- c("SPY","DIA","CORN")
#' getSymbols(Symbols)
#' length(Cl(SPY));length(Cl(DIA));length(Cl(CORN))
#' alignSymbols(Symbols)
#' length(Cl(SPY));length(Cl(DIA));length(Cl(CORN))
#' }
#' @export
alignSymbols <-
function(Symbols, env=.GlobalEnv) {
    if (length(Symbols) < 2) 
        stop("Must provide at least 2 Symbols")
    if (any(!is.character(Symbols))) 
        stop("Symbols must be vector of character strings.")
    ff <- try(get(Symbols[1], pos=env))
    if (inherits(ff, 'try-error') || 
            (!inherits(ff,'xts') && !inherits(ff,'zoo'))) {
        stop('You must create an xts object for each symbol first.')
    }
    # add the symbol and an underscore to the column names of each xts so that
    # we can "unmerge" easier
    colnames(ff) <- paste(Symbols[1], colnames(ff), sep="_") 
    ncols <- ncol(ff) # we'll make sure all Symbols ncols is same as 1st one
    symout <- Symbols[1]
    for (sym in Symbols[-1]) {
        tmp.sym <- try(get(sym,pos=env),silent=TRUE)
        if (!inherits(tmp.sym,'try-error') && 
                (inherits(tmp.sym,'xts') || inherits(tmp.sym,'zoo'))) {
            if (ncol(tmp.sym) != ncols) {
                stop("All Symbols must have the same number of columns.")
            }
            colnames(tmp.sym) <- paste(sym, colnames(tmp.sym), sep="_")
            ff <- merge(ff,tmp.sym,all=FALSE)
            ff <- na.omit(ff)
            symout <- c(symout, sym)
        }
    }
    for (sym in symout) {
        tmpid <- paste(sym, "_", sep="")
        cols <- grep(tmpid, colnames(ff))
        tmpout <- ff[, cols]
        colnames(tmpout) <- sub(tmpid, "", colnames(tmpout))
        assign(sym, tmpout, pos=env)
    }
    symout
}

