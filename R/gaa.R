#' Get, Apply, Assign
#'
#' Get data from \code{env}ironment, apply a \code{FUN}ction, and re-assign the
#' results.
#' 
#' \code{FUN} is intended to be something like \code{Cl} or \code{to.daily} as 
#' the results are stored in objects with the same name as \code{Symbols}
#'
#' @param Symbols names of xts objects
#' @param FUN quoted or unquoted name of function to be applied
#' @param env environment where data is stored (.GlobalEnv)
#' @param store.to environment in which to store the results. 
#'   By default it is the same as \code{env}
#' @param ... arguments to pass through to \code{FUN}
#' @param invisible TRUE/FALSE. If true, nothing is returned, otherwise, 
#'   \code{Symbols} will be returned
#' @seealso \code{\link{gsa}}, \code{\link{alignSymbols}}, \code{\link{PF}}
#' @author Garrett See
#' @examples
#' \dontrun{
#' s <- c('SPY', 'DIA', 'QQQ')
#' getSymbols(s, from='2011-01-01', to='2011-06-01', src='yahoo')
#' 
#' .cl <- new.env()
#' gaa(s, Cl, store.to=.cl)
#' head(.cl$SPY)
#' 
#' head(SPY)
#' gaa("SPY", adjustOHLC, symbol.name="SPY")
#' head(SPY)
#'
#' ## Need to loop for functions like adjustOHLC that use 
#' ## deparse(substitute(...)) if calling with more than 1 symbol
#' sapply(s[2:3], function(x) gaa(x, adjustOHLC, symbol.name=x))
#'
#' ## Clean up
#' rm('s', '.cl', 'SPY', 'DIA', 'QQQ')
#' }
#' @export
gaa <- function(Symbols, FUN, ..., env=.GlobalEnv, store.to=env, 
                invisible=FALSE) {
    FUN <- match.fun(FUN)
    for (sym in Symbols) {
        x <- get(sym, pos=env)
        args <- c(list(x), list(...))
        assign(sym, do.call(FUN, args), pos=store.to)
    }
    if (!isTRUE(invisible)) Symbols
}
