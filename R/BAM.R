#' Extract Bid, Ask, Mid columns or Bid, Ask, Mid, Trade columns
#' 
#' Similar to OHLC and BBO
#' 
#' @aliases BAM BATM
#' @param x xts object that includes at least the columns that are to be
#' extracted.
#' @return xts object
#' @author Garrett See, but based on -- and mostly copied -- from Jeff Ryan's
#' quantmod
#' @seealso OHLC, BBO is.BAM, is.BATM
#' @export
#' @rdname BAM
BAM <- function(x)
{
    if (is.BAM(x))
        return(x[, has.BAM(x,1)])
    NULL
}

#' @export
#' @rdname BAM
BATM <- function(x)
{
    if (is.BATM(x))
        return(x[, has.BATM(x,1)])
    NULL
}

