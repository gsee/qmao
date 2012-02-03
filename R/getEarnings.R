#' download historic earnings and earnings estimates for a given stock
#'
#' download historic earnings and earnings estimates for a given stock from
#' \url{http://earnings.com}. One of the columns of the \code{data.frame} will 
#' be the Dates/Times of the earnings release.  These Dates/Times may have one 
#' of a few different formats, most of which are ambiguous.  
#' \code{convertEarningsTime} will format the Dates/Times in an unambiguously; it
#' will be called if \code{getHistoricEarnings} is called with 
#' \code{doFormatTime = TRUE}.
#'
#' If one of the earnings Date/Time values contains \dQuote{BMO} or \dQuote{AMC},
#' it will be substituted with \dQuote{07:00:00} and \dQuote{16:15:00}, 
#' respectively in New York time.
#'
#' @param Symbol character string ticker symbol of stock
#' @param doFormatTime should the values of the Date/Time be re-formatted? (TRUE)
#' @param return.tz timezone in which to represent Date/Time of earnings release.
#'   ignored if \code{doFormatTime} is not \code{TRUE}
#' @param return.class can be one of \dQuote{xts} or \dQuote{data.frame}.  
#'
#' @param x character string representing a time or date
#' @param date.format format of the date
#' @param default.time what time to use if \code{x} is only a date. \dQuote{AMC}
#'   stands for After Market Close and will become \dQuote{16:00:00} New York
#'   time.  \dQuote{BMO} stands for Before Market Open and will become
#'   \dQuote{07:00:00} New York time.
#' @return for \code{getHistoricEarnings}, it depends on \code{return.class};
#' If it is \dQuote{xts}, an \code{xts} object will be returned that will only 
#' contain the numeric columns: 
#' dQuote{EPS.ESTIMATE}, \dQuote{EPS.ACTUAL}, and \dQuote{PREV.YEAR.ACTUAL}.
#' If \code{return.class} is \dQuote{data.frame}, a \code{data.frame} will be 
#' returned that, in addition to the columns of the xts, also contain columns 
#' \dQuote{Symbol}, \dQuote{PERIOD}, \dQuote{EVENT.TITLE}, and \dQuote{TIME}.
#' 
#' \code{convertEarningsTime} will return a string representing a date and time.
#' in the format "%Y-%m-%d %H:%M%:S %Z"
#' @references \url{http://earnings.com}
#' @seealso \code{\link{getHoldings}}, \code{\link[quantmod]{getFinancials}},
#' \code{\link[quantmod]{getDividends}}, \code{\link{get_div}},
#' \code{\link[quantmod]{getSplits}}, \code{\link{get_spl}},
#' 
#' \code{\link[base]{strftime}} for formats
#' @examples
#' \dontrun{
#' getEarnings('GOOG')
#' getEarnings("GOOG", return.class='data.frame')
#' getEarnings("GOOG", return.class='data.frame', doFormatTime=FALSE)
#' }
#' @export
#' @rdname getEarnings
getEarnings <- function(Symbol, 
                        doFormatTime=TRUE, 
                        return.tz='America/Chicago',
                        return.class=c('xts', 'data.frame')) {
    require(XML)
    return.class <- return.class[[1]]
    if (!return.class %in% c("xts", "data.frame")) 
        stop('return.class must be either "xts" or "data.frame"')
    URL <- paste("http://earnings.com/company.asp?client=cb&ticker=", Symbol, sep="")
    x <- readHTMLTable(URL, stringsAsFactors=FALSE)
    table.loc <- tail(grep("Earnings Releases", x), 1) + 1
    if (identical(numeric(0), table.loc)) return(NULL)
    df <- x[[table.loc]]
    header <- df[1, ]
    df <- df[-1, ]
    colnames(df) <- header
    #format ticker column
    df[, 1] <- gsub("\r\n\t\t\t", "", df[, 1])
    df <- na.omit(df)
    df[df == "n/a\uA0"] <- NA #on Ubuntu there is a non-break space
    df[df == "n/a"] <- NA #on Mac there is neither a space nor a non-break space
    #format Date/Time column
    # AMC means after mkt close, which I'll interpret to mean 16:15 ET
    # BMO means before mkt open, which I'll interpret to mean 07:00 ET
    dt <- df[, grep("DATE/TIME", colnames(df))]
    default.time <- if (any(grepl("AMC", dt))) {
        "AMC"
    } else if (any(grepl("BMO", dt))) {
        "BMO"
    } else "AMC"

    if (!isTRUE(doFormatTime) && return.class == "xts") {
        warning('doFormatTime argument will be ignored for "xts" return.class')
        doFormatTime <- TRUE
    }
    if (isTRUE(doFormatTime)) {
        DTcol <- grep("DATE/TIME", colnames(df))
        colnames(df)[DTcol] <- "TIME"
        df[, DTcol] <- do.call(c, 
        lapply(dt, convertEarningsTime, 
            date.format='%d-%b-%y', 
            default.time=default.time, 
            return.tz=return.tz))
    }
    # Convert dollars to numeric
    dSignCols <- grep("\\$", df)
    for(dS in dSignCols) {
        df[, dS] <- gsub(" ", "", df[, dS])
        df[, dS] <- gsub("\uA0", "", df[, dS])
        df[, dS] <- as.numeric(gsub("\\$", "", df[, dS]))
    }
    cn <- make.names(colnames(df))
    #colnames PREV..YEAR.ACTUAL --> PREV.YEAR.ACTUAL
    cn <- gsub('..', '.', cn, fixed=TRUE) 
    colnames(df) <- cn
    if (return.class == 'data.frame') return(df)
    if (return.class == 'xts') return(xts(df[, grep("EPS|ACTUAL", cn)], 
                                          as.POSIXct(df[, "TIME"])))
}

#' @export
#' @rdname getEarnings
convertEarningsTime <- function(x, 
                                date.format='%d-%b-%y', 
                                default.time="AMC",
                                return.tz='America/Chicago') {
    # There are a few different formats; here are some *examples*
    # [1] 9-Apr-12 - 14-Apr-12
    # [2] 13-Jan-12 7:00 AM
    # [3] 17-Apr-02 4:15 PM
    # [4] 15-Jan-09 BMO
    # [5] 19-Jan-12 AMC
    # AMC means after mkt close, which I'll interpret to mean 16:15 ET
    # BMO means before mkt open, which I'll interpret to mean 07:00 ET
    default.time <- switch(default.time, 
                           AMC = '16:15:00', 
                           BMO = '07:00:00',
                           default.time)
    #dchr <- gsub(" AMC| BMO", "", x)
    # Get Date and Time
    if (any(grepl(" AMC| BMO", x))) {
        x <- gsub(" AMC", " 16:15:00", x) #if it has AMC, replace it with time
        x <- gsub(" BMO", " 07:00:00", x) #if it has BMO, replace it with time
    }
    formatTime <- function(x, ...) {
        format(as.POSIXct(x, 
                          tz = 'America/New_York', 
                          origin = as.Date("1970-01-01"),
                          ...),
              tz=return.tz, usetz=TRUE)
    }     
    if (length(grep("-", strsplit(x, "")[[1]])) > 2) {
        #x <- "9-Apr-12 - 14-Apr-12"
        warning('Using 1st date of earnings date range')
        return(formatTime(paste(as.Date(strsplit(x, " - ")[[1]][1], 
                                format=date.format), 
                         default.time)))
    } else if (any(grepl(" AM| PM", x))) {
        # x <- "13-Jan-12 7:00 AM"
        # x <- "18-Apr-00 7:30 AM"
        # x <- "17-Apr-02 4:15 PM"
        return(formatTime(x, format="%d-%b-%y %I:%M %p"))
    } else {
        if (nchar(x) < 12) x <- paste(x, default.time)
        return(formatTime(x, format=paste(date.format, "%H:%M:%S")))
    }
}

#' download historic earnings and earnings estimates for a given stock
#'
#' THIS FUNCTION HAS BEEN RENAMED; Use \code{\link{getEarnings}} instead. 
#' 
#' download historic earnings and earnings estimates for a given stock from
#' \url{http://earnings.com}. One of the columns of the \code{data.frame} will 
#' be the Dates/Times of the earnings release.  These Dates/Times may have one 
#' of a few different formats, most of which are ambiguous.  
#' \code{convertEarningsTime} will format the Dates/Times in an unambiguously; it
#' will be called if \code{getHistoricEarnings} is called with 
#' \code{doFormatTime = TRUE}.
#'
#' If one of the earnings Date/Time values contains \dQuote{BMO} or \dQuote{AMC},
#' it will be substituted with \dQuote{07:00:00} and \dQuote{16:15:00}, 
#' respectively in New York time.
#'
#' @param Symbol character string ticker symbol of stock
#' @param doFormatTime should the values of the Date/Time be re-formatted? (TRUE)
#' @param return.tz timezone in which to represent Date/Time of earnings release.
#'   ignored if \code{doFormatTime} is not \code{TRUE}
#' @param return.class can be one of \dQuote{xts} or \dQuote{data.frame}.  
#' @return for \code{getHistoricEarnings}, it depends on \code{return.class};
#' If it is \dQuote{xts}, an \code{xts} object will be returned that will only 
#' contain the numeric columns: 
#' dQuote{EPS.ESTIMATE}, \dQuote{EPS.ACTUAL}, and \dQuote{PREV.YEAR.ACTUAL}.
#' If \code{return.class} is \dQuote{data.frame}, a \code{data.frame} will be 
#' returned that, in addition to the columns of the xts, also contain columns 
#' \dQuote{Symbol}, \dQuote{PERIOD}, \dQuote{EVENT.TITLE}, and \dQuote{TIME}.
#' 
#' \code{convertEarningsTime} will return a string representing a date and time.
#' in the format "%Y-%m-%d %H:%M%:S %Z"
#' @references \url{http://earnings.com}
#' @seealso \code{\link{getHoldings}}, \code{\link[quantmod]{getFinancials}},
#' \code{\link[quantmod]{getDividends}}, \code{\link{get_div}},
#' \code{\link[quantmod]{getSplits}}, \code{\link{get_spl}},
#' 
#' \code{\link[base]{strftime}} for formats
#' @examples
#' \dontrun{
#' getHistoricEarnings('GOOG')
#' getHistoricEarnings("GOOG", return.class='data.frame')
#' getHistoricEarnings("GOOG", return.class='data.frame', doFormatTime=FALSE)
#' }
getHistoricEarnings <- function(Symbol, 
                                doFormatTime=TRUE, 
                                return.tz='America/Chicago',
                                return.class=c('xts', 'data.frame')) {
    .Deprecated("getEarnings")
}
