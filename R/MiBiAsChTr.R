
Mi <- function(x) {
    if (has.Mid(x)) 
        return(x[, grep("Mid", colnames(x), ignore.case = TRUE)])
    stop("subscript out of bounds: no column name containing \"Mid\"")
}
Bi <- function(x) {
    if (has.Bid(x)) 
        return(x[, grep("Bid", colnames(x), ignore.case = TRUE)])
    stop("subscript out of bounds: no column name containing \"Bid\"")
}
As <- function(x) {
    if (has.Ask(x)) 
        return(x[, grep("Ask", colnames(x), ignore.case = TRUE)])
    stop("subscript out of bounds: no column name containing \"Ask\"")
}
Ch <- function(x) {
    if (has.Chg(x)) 
        return(x[, grep("Chg", colnames(x), ignore.case = TRUE)])
    stop("subscript out of bounds: no column name containing \"Chg\"")
}
Tr <- function(x) {
	if (has.Trade(x))
		return(x[, grep("Trade", colnames(x),ignore.case=TRUE)])
	stop("subscript out of bounds: no column name containing \"Trade\"")
}

