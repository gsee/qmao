
#' Extract Columns of Data from and xts object
#'
#' Extract "Bid", "Ask", "Mid", "Trade", and "Change" columns
#'
#' These are similar to the quantmod functions \code{\link{Op}}, \code{\link{Hi}},
#' \code{\link{Lo}}, \code{\link{Cl}}, \code{\link{Ad}}.  They use grep
#' to locate the appropriate columns, and are case insensitive.
#'
#' \code{Mi} will calculate the Mid column if it does not exist, but Bid and Ask columns do exist.
#'
#' @param x a data object with columns containing data to be extracted
#' @param symbol.name used only if the Mid column has to be calculated for creating the column name.
#' @return an xts object with the appropriately named column
#' 
#' @export
#' @rdname MiBiAsChTr
Mi <- function(x, symbol.name=NULL) {
    if (has.Mid(x)) 
        return(x[, grep("Mid", colnames(x), ignore.case = TRUE)])
    if (is.BBO(x)) {
        FindPrice <- function(xx) {
            if (length(xx) > 1) {
                xx <- xx[, -grep("qty|quantity|size", colnames(xx), 
                                 ignore.case=TRUE), drop=FALSE]
            }
            if (length(xx) > 1) {
                if (any(grepl("Price", colnames(xx), ignore.case=TRUE))) {
                    xx[, grep("Price", colnames(xx), ignore.case=TRUE)[1],
                       drop=FALSE]
                } else xx[, 1, drop=FALSE]
            } else xx
        }
        mid <- (Bi(FindPrice(Bi(x))) + As(FindPrice(As(x)))) / 2
        if (is.null(symbol.name)) {
            tmpsym <- strsplit(colnames(Bi(x)[,1]), "\\.")[[1]][1]
            if (identical(integer(0), grep("bid|ask", tmpsym, ignore.case=TRUE))) 
                symbol.name <- tmpsym
            else symbol.name <- NULL
        }
        if (is.null(symbol.name)) { sep="" } else sep="."
        colnames(mid) <- paste(symbol.name, "Mid.Price", sep=sep)
        return(mid)
    }
    stop("subscript out of bounds: no column name containing \"Mid\"")
}

#' @export
#' @rdname MiBiAsChTr
Bi <- function(x) {
    if (has.Bid(x)) 
        return(x[, grep("Bid", colnames(x), ignore.case = TRUE)])
    stop("subscript out of bounds: no column name containing \"Bid\"")
}

#' @export
#' @rdname MiBiAsChTr
As <- function(x) {
    if (has.Ask(x)) 
        return(x[, grep("Ask", colnames(x), ignore.case = TRUE)])
    stop("subscript out of bounds: no column name containing \"Ask\"")
}

#' @export
#' @rdname MiBiAsChTr
Ch <- function(x) {
    if (has.Chg(x)) 
        return(x[, grep("Chg", colnames(x), ignore.case = TRUE)])
    stop("subscript out of bounds: no column name containing \"Chg\"")
}

#' @export
#' @rdname MiBiAsChTr
Tr <- function(x) {
	if (has.Trade(x))
		return(x[, grep("Trade", colnames(x),ignore.case=TRUE)])
	stop("subscript out of bounds: no column name containing \"Trade\"")
}

