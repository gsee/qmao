
#' Extract Columns of Data from and xts object
#'
#' Extract "Bid", "Ask", "Mid", "Trade", and "Change" columns
#'
#' These are similar to the quantmod functions \code{\link{Op}}, \code{\link{Hi}},
#' \code{\link{Lo}}, \code{\link{Cl}}, \code{\link{Ad}}.  They use grep
#' to locate the appropriate columns, and are case insensitive.
#'
#' @param x a data object with columns containing data to be extracted
#' @return an xts object with the appropriately named column
#' 
#' @export
#' @rdname MiBiAsChTr
Mi <- function(x) {
    if (has.Mid(x)) 
        return(x[, grep("Mid", colnames(x), ignore.case = TRUE)])
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

