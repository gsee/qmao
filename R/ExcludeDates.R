#' Exclude one or more days from an xts object
#' 
#' remove one or more days of data from an xts object
#' 
#' @param x An xts object
#' @param exclude character vector of dates
#' @return an xts object that does not contain any rows from the dates specified
#' by \code{exclude}
#' @author Garrett See
#' @note be careful passing POSIXct objects to exclude as 
#' \code{as.Date.POSIXct} has \code{tz="UTC"} hardcoded.
#' @examples
#' data(sample_matrix)
#' x <- as.xts(sample_matrix)
#' head(ExcludeDates(x, exclude = c("2007-01-05", "2007-01-03")))
#' @export
ExcludeDates <- function(x, exclude) {
    idx <- index(x)
    x[idx[!format(idx, "%Y-%m-%d") %in% paste(exclude)]]
}
