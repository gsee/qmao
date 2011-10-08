#' Get, Subset, Assign
#'
#' Get data from \code{env}ironment, subset the data, and re-assign it.
#'
#' Primarily intended to remove data from outside of
#' regular trading hours.  However, can be used for
#' any xts style subsetting.
#'
#' @param Symbols names of xts objects
#' @param subset xts style subetting string. ("T08:30/T15:00")
#' @param env environment where data is stored (.GlobalEnv)
#' @param store.to environment in which to store the subsetted data. By default it is the same as \code{env}
#' @param invisible TRUE/FALSE. If true, nothing is returned, otherwise, the name of symbols that were subset will be returned
#' @seealso \code{\link{alignSymbols}}
#' @author Garrett See
#' @examples
#' \dontrun{
#' getSymbols('SPY', from='2011-01-01', to='2011-06-01', src='yahoo')
#' tail(SPY)
#' gsa("SPY","::2011-04-15")
#' tail(SPY)
#' }
#' @export
gsa <- function(Symbols, subset='T08:30/T15:00', env=.GlobalEnv, store.to=env, invisible=FALSE) {
    if (is.character(env)) env <- get(env, pos=.GlobalEnv)
    if (is.character(store.to)) store.to <- get(store.to, pos=.GlobalEnv)    
    stopifnot(is.environment(env), is.environment(store.to))
    symout <- NULL
    intradayss <- if (is.character(subset) && substr(subset, 1, 1) == "T") {TRUE} else {FALSE}
    for (sym in Symbols) {
        xx <-  try(get(sym, pos=env))
        if (!inherits(xx, 'try-error')) {
            if (!(intradayss && periodicity(xx)$frequency >= 86400)){ 
                assign(sym, xx[subset], pos=store.to)
                symout <- c(symout, sym)
            } else warning('intraday subset not applied to non-intraday data.')
        }
    }
    if (invisible) return(invisible())
    symout
}

