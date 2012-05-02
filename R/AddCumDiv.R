#' Add dividends back to price series
#' 
#' At every timestamp, add to the prices the cumulative amount of dividends 
#' receieved since the beginning of the dataset.
#'
#' After a series has been adjusted, an "adj" attr of TRUE is added to the 
#' object.  If the object already has an "adj" attr, the original object 
#' \code{x} will be returned and no adjustment will be made.
#'
#' cumulative dividends will only be added to columns with names that include 
#' one of the following terms: Open, High, Low, Close, Bid.Price, Ask.Price, 
#' Trade.Price, Mid.Price.
#'
#' TODO: make a function that does the opposite of this one.
#' @param x xts or character name of xts object
#' @param name name of stock to use when getting dividends
#' @param env environment in which to find \code{x}
#' @return xts object with same dims as \code{x} that is the original \code{x}
#' with cumulative dividends added.  An \code{attr} called \dQuote{cumdiv} that 
#' contains an xts of the cumulative dividends will be added to the returned 
#' object
#' @author Garrett See
#' @examples 
#' \dontrun{
#' ## You must setDefaults on getSymbols.FI (at least for the "dir" argument)
#' ## for this to work
#' getSymbols("SPY", src='FI', from='2011-09-01', to='2011-12-31')
#' SPY.tp <- AddCumDiv(SPY)
#' tail(attr(SPY.tp, "cumdiv"))
#' cbind(tail(estAd(SPY)), estAd(tail(SPY.tp)))
#' }
#' @export
AddCumDiv <- function(x, name, env=.GlobalEnv) {
    # adds cumulative dividends to prices
    if (missing(name)) {
        name <- if (!is.character(x)) {
            deparse(substitute(x))
        } else x
    } 
    if (is.character(x)) x <- get(x, pos=env)
    stopifnot(is.xts(x))
    if (isTRUE(attr(x, "adj"))) {
        # at the end of this function, x is given an "adj" attr with a value of
        # TRUE. Check here.  If there is an 'adj' attr that is TRUE, then this 
        # data has already been adjusted
        warning(paste(name, 'has already been adjusted.', 
            'Set attr(, "adj") to FALSE to allow adjusting again. \n'))
        return(x)
    }
    cumdiv <- function(x, name) {
        div <- get_div(name)[paste(format(start(x), "%Y-%m-%d", 
                                          tz=Sys.getenv("TZ")), 
                                   "/", format(end(x), "%Y-%m-%d", 
                                               tz=Sys.getenv("TZ")), 
                                   sep="")]
        if (length(div) == 0) div <- 0
        divs <- cbind(x, div)[, NCOL(x) + 1]
        divs[is.na(divs)] <- 0
        out <- cumsum(divs)
        out[index(x)] #cumsum(divs) prob. has more rows because dividend payments 
        #have a timestamp at "midnight"
    }
    x.cd <- cumdiv(x, name)
    # set a flag so that if this function is called 
    # again, it will not try to adjust something
    # that has already been adjusted.
    attr(x, 'adj') <- TRUE 
    attr(x, "cumdiv") <- if (any(x.cd > 0)) {
        xts(x.cd[x.cd > 0], index(x.cd[x.cd > 0]))
    } else 0
    # Only add dividends to Open, High, Low, Close, Bid.Price, Ask.Price,
    # Trade.Price, Mid.Price   

    cn <- grep("Open|High|Low|Close|Bid.Price|Ask.Price|Trade.Price|Mid.Price", 
               colnames(x))
    #add cumulative divs to prices; don't change non-prices
    # we'll get a warning if cn is all columns, or no columns; suppress it
    out <- suppressWarnings(cbind(sweep(x[, cn], 1, x.cd, "+"), x[, -cn]))
    out[, colnames(x)] # put back in original order
}
