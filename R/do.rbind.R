#' Split into \code{f} [days], apply a function to each
#' \code{f} [day], and rbind the results together
#'
#' this is a simple convenience function for this type of
#' usage:
#' \code{do.call(rbind, lapply(split(x, f)), FUN)}
#'
#' From the \code{\link[xts]{endpoints}} help page:
#' Valid values for the argument \code{f} include: \dQuote{us} (microseconds),
#' \dQuote{microseconds}, \dQuote{ms} (milliseconds),
#' \dQuote{milliseconds}, \dQuote{secs} (seconds),
#' \dQuote{seconds}, \dQuote{mins} (minutes), \dQuote{minutes},
#' \dQuote{hours}, \dQuote{days}, \dQuote{weeks}, \dQuote{months}, \dQuote{quarters},
#' and \dQuote{years}.
#'
#' @param x xts object
#' @param FUN function to apply to each period
#' @param f string describing the period to split by
#' @param indexAt Currently only "lastof" (Default) and "firstof" are supported.  
#' The index will be the last (first) time of each period.
#' @return xts
#' @author Garrett See
#' @examples
#' data(sample_matrix)
#' x <- as.xts(sample_matrix)
#' do.rbind(x, NROW, 'weeks')
#' do.rbind(x, last, 'weeks')
#' @export
do.rbind <- function(x, FUN, f='days', indexAt='lastof') {
    x <- try.xts(x)
    FUN <- match.fun(FUN)
    switch(indexAt, 
        lastof=, endof={
            idx <- x[endpoints(x, f)]
            if (identical(FUN, last)) return(idx)
        },
        firstof=, startof={
            idx <- x[xts:::startof(x, f)]
            if (identical(FUN, first)) return(idx)
        })    
    xts(do.call(rbind, lapply(split(x, f), FUN)), index(idx))
}
