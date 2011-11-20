#' Extract Bid, Ask, Mid columns or Bid, Ask, Mid, Trade columns
#' 
#' Similar to OHLC, but if the \sQuote{Mid} column does not exist,
#' it will be calculated.
#' 
#' @aliases BAM BATM
#' @param x xts object that includes at least the columns that are to be
#' extracted.
#' @param symbol.name used only if the Mid column has to be calculated for creating the column name.
#' @return xts object
#' @author Garrett See, but based on -- and mostly copied from -- Jeff Ryan's
#' quantmod
#' @seealso OHLC, BBO is.BAM, is.BATM
#' @export
#' @rdname BAM
BAM <- function(x, symbol.name=NULL)
{
    if (is.BAM(x))
        return(x[, has.BAM(x,1)])
    if (is.BBO(x)) {
        out <- cbind(x[, c(has.Bid(x,1)[1], has.Ask(x,1)[1])])
        mid <- (out[,1] + out[,2])/2
        if (is.null(symbol.name)) {
            tmpsym <- strsplit(colnames(out[1]), "\\.")[[1]][1]
            if (identical(integer(0), grep("bid|ask", tmpsym, ignore.case=TRUE))) 
                symbol.name <- tmpsym
            else symbol.name <- NULL
        }
        if (is.null(symbol.name)) { sep="" } else sep="."
        colnames(mid) <- paste(symbol.name, "Mid.Price", sep=sep)
        out <- cbind(out, mid)
        return(out)    
    }
    NULL
}

#' @export
#' @rdname BAM
BATM <- function(x, symbol.name=NULL)
{
    if (is.BATM(x))
        return(x[, has.BATM(x,1)])
    if (all(has.Bid(x), has.Ask(x), has.Trade(x))) {
        #calculate Mid
        out <- x[, c(has.Bid(x,1)[1], has.Ask(x,1)[1], has.Trade(x,1)[1])]
        mid <- (out[,1] + out[,2])/2
        if (is.null(symbol.name)) {
            tmpsym <- strsplit(colnames(out[1]), "\\.")[[1]][1]
            if (identical(integer(0), grep("bid|ask", tmpsym, ignore.case=TRUE))) 
                symbol.name <- tmpsym
            else symbol.name <- NULL
        }
        if (is.null(symbol.name)) { sep="" } else sep="."
        colnames(mid) <- paste(symbol.name, "Mid.Price", sep=sep)
        out <- cbind(out, mid)
        return(out)
    }
    NULL
}

