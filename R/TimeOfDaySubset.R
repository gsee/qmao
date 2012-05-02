#' Subset by time-of-day
#' 
#' If the first time in the string is later in the day than the first time,
#' the data between those times will be removed.  Otherwise, \code{x[timespan]}
#' will be returned.
#' @param x xts data object
#' @param timespan string (e.g. \dQuote{T08:30/T15:00} or 
#'   \dQuote{"T17:00/T15:15"})
#' @return an extraction of the original xts object that includes either
#'   times betweeen the first and last time of \code{timespan} if the first
#'   time comes earlier in the day than the second, or times that are not
#'   between the last and first time if the the last time comes earlier in the
#'   day than the first time.
#' @author Garrett See
#' @examples
#' x <- xts(1:10080, Sys.time() + 60 * 1:10080)
#' NROW(aggregate(x, format(index(x), "%H"), length)) # number of hours that have data
#' x1 <- TimeOfDaySubset(x, "T08:30/T14:59.999")
#' NROW(aggregate(x1, format(index(x1), "%H"), length))
#' x2 <- TimeOfDaySubset(x, "T18:00/T13:00.000")
#' NROW(aggregate(x2, format(index(x2), "%H"), length))
#' @export
TimeOfDaySubset <- function(x, timespan) {
    if (timespan == "") return(x)
    if (substr(timespan, 1, 1) != "T" || !all(isTRUE(grepl("/T", timespan)))) {
        stop('timespan should be formatted like "T15:00:00/T16:00:00"')
    }
    #ts <- gsub("T", "", timespan)
    pts <- strsplit(timespan, "/")[[1]]
    ptspan <- .parseISO8601(timespan) #parsed timespan
    if (ptspan[["first.time"]] + 60 * 60 * 24 * 364 < ptspan[["last.time"]]) {
        return(x[timespan])
    } else {
        return(x[!index(x) %in% index(x[paste(pts[2], pts[1], sep="/")])])
        ## The following be would inclusive -- T17:15/T17:00 would include those 
        ## times 17:15 and 17:00, whereas above code does not. The obove is 
        ## also at least twice as fast as below.
        #return(rbind(x[paste(pts[1L], "T23:59:59.99999", sep="/")],
        #             x[paste("T00:00/T", pts[2], sep="")]))
    } 
}
