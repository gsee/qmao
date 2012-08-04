
#' Get a calendar over more than one time period from yahoo's website
#' 
#' Given a \code{from} and \code{to} argument, download a calendar from every
#' time period between those Dates, and merge them together into a 
#' \code{data.frame}.
#'
#' These functions take a function that is designed to only download a calendar 
#' over a single timeperiod.  They then apply that function over as many 
#' timeperiods as necessary to create a complete calendar from \code{from} to 
#' \code{to}.
#' 
#' Currently, 
#' \code{\link{.getEarningsCalendar}} and 
#' \code{\link{.getEconomicCalendarBriefing}} are used by 
#' \code{getCalendarByDay}, \code{\link{.getEconomicCalendarYahoo}}  is used by 
#' \code{getCalendarByWeek}, and 
#' \code{\link{.getMergersCalendar}} is used by \code{getCalendarsByMonth}
#' 
#' Hopefully new ones will be written soon; Splits are in the works (which will
#' use \code{getCalendarByMonth} as well as 
#' Earnings Surprises (which will use getCalendarByDay.)
#'
#' @param FUN a function to be applied to all time periods between \code{from} 
#'   and \code{to}.
#' @param from first time period contains this Date
#' @param to last time period contains this Date
#' @note ALPHA CODE: Subject to change!
#' @author Garrett See
#' @references 
#' \url{http://biz.yahoo.com/research/earncal/today.html}
#' 
#' \url{http://biz.yahoo.com/c/e.html}
#' @seealso \code{\link{getEconomicCalendar}}, \code{\link{getEarningsCalendar}}
#' @examples
#' \dontrun{
#' getCalendarByDay('.getEconomicCalendar')
#' getCalendarByWeek('.getEarningsCalendar')
#' getCalendarByMonth('.getMergersCalendar', '2012-06-01')
#' }
#' @rdname getCalendarBy
getCalendarByDay <- function(FUN, from, to) {
  FUN <- match.fun(FUN)
  if (missing(from) && missing(to)) {
    return(FUN())
  } else {
    if(missing(to)) { to <- from }
    if (missing(from)) { from <- to }
    from <- as.Date(from)
    to <- as.Date(to)
    s <- seq(from, to, by='days')
    #s <- as.Date(from:to)
    s <- s[!weekdays(s) %in% c("Saturday", "Sunday")]
    out <- do.call(rbind, lapply(s, FUN))
    rownames(out) <- NULL
    out
  }
}

#' @rdname getCalendarBy
getCalendarByWeek <- function(FUN, from, to) {
  FUN <- match.fun(FUN)
  if (missing(from) && missing(to)) {
    #YW <- format(Sys.Date(), "%Y%W")
    return(FUN())
  } else {
    if(missing(to)) { to <- from } #Sys.Date() }
    if (missing(from)) { from <- to } #as.Date(to) - 5 }
    from <- as.numeric(format(as.Date(from), "%Y%W"))
    to <- as.numeric(format(as.Date(to), "%Y%W"))
    s <- seq(from, to)
    s <- s[!substr(s, 5, 6) == "00"]
    ss <- split(s, substr(s, 1, 4))
    YW <- unlist(lapply(names(ss), function(ni) {
      maxweeknum <- as.numeric(format(as.Date(paste(ni, "12-31", sep="-")), "%Y%W"))
      ss[[ni]][ss[[ni]] <= maxweeknum]
    }))
  }
  out <- lapply(YW, FUN)
  do.call(rbind, out)
}

#' @rdname getCalendarBy
getCalendarByMonth <- function(FUN, from, to) {
  FUN <- match.fun(FUN)
  if (missing(from) && missing(to)) {
    #YW <- format(Sys.Date(), "%Y%W")
    return(FUN())
  } else {
    if(missing(to)) { to <- from } #Sys.Date() }
    if (missing(from)) { from <- to } #as.Date(to) - 5 }
    #from <- as.numeric(format(as.Date(from), "%Y%m"))
    #to <- as.numeric(format(as.Date(to), "%Y%m"))
    from <- as.Date(from)
    to <- as.Date(to)
    s <- unique(format(seq(from, to, by='days'), "%Y%m"))
    out <- lapply(s, FUN)
    return(do.call(rbind, out))
  }
}

#===============================================================================

#' Get the Economic Calender from Briefing.com or Yahoo.
#' 
#' Download the Briefing.com economic calendar either directly from the 
#' briefing.com website, or via yahoo's website, and create 
#' a data.frame containing information about previous and/or scheduled 
#' realeases of economic economic indicators. The returned \code{data.frame} 
#' will have a \code{Time} column that contains an intraday timestamp with 
#' a time zone of \code{America/New_York}.
#' 
#' \code{.getEconomicCalendarYahoo} will retrieve the Economic Calendar from 
#' Yahoo for a single week.  Use this function if you need to get the economic
#' calendar of scheduled releases (i.e. have not yet occurred).  The time of
#' day is not always correct for historic releases.  Therefore, if you are
#' getting Dates from the past, it is recommended that you use 
#' \code{.getEconomicCalendarBriefing} or \code{getEconomicCalendarBriefing}
#' 
#' \code{.getEconomicCalendarBriefing} will retrieve the Economic Calendar from
#' Briefing.com for a single day.  This function does not support Dates that are
#' in the future, but is more reliable for past Dates than the data obtained
#' via yahoo. (Briefing provides the data to yahoo, so I'm not sure why there
#' are discrepancies.)
#'
#' \code{getEconomicCalendarYahoo} is a wrapper that accepts both a \code{from} 
#' and \code{to} argument.  It will use \code{\link{getCalendarByWeek}} to 
#' make repeated calls to \code{.getEconomicCalendarYahoo} allowing for the 
#' retrieval of an Economic Calendar over a much longer timespan.
#' 
#' for \code{getEconomicCalendarBriefing}, \code{from} and \code{to} are the 
#' first and last dates that should be included in the returned 
#' \code{data.frame}.  However, for \code{getEconomicCalendarYahoo}, \code{from} 
#' and \code{to} are used to pick the first and last \emph{week} to
#' download.  If \code{from} is a Date that is a Wednesday, the first data will
#' be from previous Monday.  Likewise, if \code{to} is a Date that is a 
#' Wednesday, the last data will be from the Friday of that week.
#'
#' \code{getEconomicCalendar} is a wrapper that will call 
#' \code{getEconomicCalendarYahoo} if any of the requested Dates occur in the 
#' future; otherwise, it will call \code{getEconomicCalendarBriefing}.
#' 
#' @param YW Used with \code{getEconomicCalendarYahoo}, a six character string 
#'   with the first 4 characters representing the year and the last 2 characters 
#'   representing the week of the year. For example, \dQuote{201217} would be 
#'   the 17th week of 2012.
#' @param Date used with \code{getEconomicCalendarBriefing}, a Date for which
#'   to retrieve the Economic Calendar.
#' @param from Date that is in the earliest week to retrieve.
#' @param to Date that is in the last week to retrieve.
#' @return a data.frame containing the economic calendar for the time period
#'   specified by \code{YW} or \code{Date}, or for all time periods between and 
#'   including \code{from} and \code{to}. It will have columns:
#'
#'   \item{Time}{POSIXct object with \code{America/New_York} time zone}
#'   \item{Statistic}{Description of the data being released}
#'   \item{For}{non-standard character string indicating the time period over 
#'     which the \code{Statistic} was measured. Could be things like Mar, 
#'     05/26, Q1, April, etc.)}
#'   \item{Actual}{Actual observed value of the \code{Statistic}}
#'   \item{Briefing.Forecast}{the value that Briefing.com predicted the 
#'     \code{Statistic} to be}
#'   \item{Market.Expects}{Consensus forecast}
#'   \item{Prior}{prior reading}
#' @author Garrett See
#' @references \url{http://biz.yahoo.com/c/e.html}
#' @note ALPHA CODE!!! Subject to change.
#' @seealso \code{\link{getEarningsCalendar}}, 
#'   \code{\link{getCalendarByWeek}}
#' @examples
#' \dontrun{
#' .getEconomicCalendarBriefing()
#' .getEconomicCalendarBriefing('2012-06-01')
#'
#' .getEconomicCalendarYahoo()
#' .getEconomicCalendarYahoo(201117)
#' .getEconomicCalendarYahoo("201117") #same
#'
#' getEconomicCalendarYahoo(from='2012-06-04', to='2012-06-10') #only goes through Friday 2012-06-08
#' getEconomicCalendarYahoo(from='2012-06-04', to='2012-06-11') #goes through Friday 2012-06-15
#'
#' getEconomicCalendarBriefing(from='2012-06-04', to='2012-06-12') #only goes to 'to' Date
#'
#' getEconomicCalendar(from=Sys.Date()-5, to=Sys.Date() - 1) #uses Briefing.com because it's Dates from past
#' getEconomicCalendar(from=Sys.Date(), to=Sys.Date() + 5) #uses Yahoo because it's Dates from future
#' }
#' @export
#' @rdname getEconomicCalendar
.getEconomicCalendarYahoo <- function(YW=format(Sys.Date(), "%Y%W")) {
  require(XML)
  stopifnot(length(YW) == 1)
  if (is.timeBased(YW) || nchar(YW) == 10) {
    YW <- format(as.Date(YW), "%Y%W")
  } else if (nchar(YW) == 5) {
    YW <- paste0(substr(YW, 1, 4), 0, substr(YW, 5, 5))
  } 
  if (nchar(YW) != 6) stop("'YW' should be 6 digits or a Date")
  Y <- substr(YW, 1, 4)
  rt <- try(readHTMLTable(paste0("http://biz.yahoo.com/c/ec/", YW, ".html"), 
                      stringsAsFactors=FALSE), silent=TRUE)
  if (inherits(rt, 'try-error')) { return(NULL) }
  dat <- rt[[which.max(sapply(rt, nrow))]]
  colnames(dat) <- make.names(dat[1, ])
  dat <- dat[-1, ]
  #read.zoo(dat, index.column=1:2
  cal <- data.frame(Time=as.POSIXct(paste(Y, dat[, 1], dat[, 2]), 
                                    format="%Y %b %d %I:%M %p", 
                                    tz='America/New_York'), 
                    dat[, -c(1, 2)])
  cal
}

#' @export
#' @rdname getEconomicCalendar
.getEconomicCalendarBriefing <- function(Date=Sys.Date()) {
  require("XML")
  Date <- as.Date(Date)
  Y <- format(Date, "%Y")
  if (Date > Sys.Date()) {
    stop(paste0(".getEconomicCalendarBriefing does not support Dates from the",
                " future. Use .getEconomicCalendarYahoo instead."))
  }
  rt <- try(readHTMLTable(paste0("http://briefing.com/investor/calendars/economic/",
                                 format(Date, "%Y/%m/%d")), 
                          stringsAsFactors=FALSE), silent=TRUE)
  if (inherits(rt, 'try-error')) { return(NULL) }
  dat <- rt[[tail(grep("Release", rt), 1)]]
  #dat <- rt[[which.max(sapply(rt, nrow))]]
  dat <- dat[-1, ]
  if (NROW(dat) == 0) { return(NULL) }
  cal <- data.frame(Time=as.POSIXct(paste(Y, dat[, 1], dat[, 2]), 
                                    format="%Y %b %d %H:%M", 
                                    tz='America/New_York'), 
                    dat[, -c(1, 2)])
  colnames(cal) <- c("Time", "Statistic", "For", "Actual", "Briefing.Forecast", 
                     "Market.Expects", "Prior", "Revised.From")
  cal
}

#' @export
#' @rdname getEconomicCalendar
getEconomicCalendarYahoo <- function(from, to) {
  getCalendarByWeek(".getEconomicCalendarYahoo", from, to)
}

#' @export
#' @rdname getEconomicCalendar
getEconomicCalendarBriefing <- function(from, to) {
  if (missing(from) && missing(to)) {
    return(getCalendarByDay(".getEconomicCalendarBriefing"))
  } else {
    if(missing(to)) { to <- from }
    if (missing(from)) { from <- to }
  }
  to <- as.Date(to)
  if (to > Sys.Date()) { 
    stop(paste0("'getEconomicCalendarBriefing' does not support Dates from",
                " the future. Use 'getEconomicCalendarYahoo' instead."))
  }
  getCalendarByDay(".getEconomicCalendarBriefing", from=from, to=to)
}

#' @export
#' @rdname getEconomicCalendar
getEconomicCalendar <- function(from, to) {
  if (missing(from) && missing(to)) {
    return(getCalendarByDay(".getEconomicCalendarYahoo"))
  }
  if (missing(to)) { to <- from }
  if (missing(from)) { from <- to }
  from <- as.Date(from)
  to <- as.Date(to)
  # if both dates are in the past, use Briefing, otherwise, use Yahoo
  if (from <= to && to < Sys.Date()) { 
    return(getEconomicCalendarBriefing(from=from, to=to))
  } else return(getEconomicCalendarYahoo(from=from, to=to))
}


#' Get the earnings calendar from yahoo
#' 
#' Get a data.frame of all the stocks that announce(d) earnings on a given Date.
#' 
#' \code{.getEarningsCalendar} will usually be called by 
#' \code{\link{getCalendarByDay}}, but it can also be called directly.
#' 
#' \code{getEarningsCalendar} is a wrapper that creates a sequence of dates 
#' between \code{from} and \code{to}, and then applies 
#' \code{.getEarningsCalendar} to each of those dates and, finally, merges the 
#' results together into a single \code{data.frame}.
#' 
#' @param Date a Date, or character string in the format CCYY-MM-DD
#' @param from first Date for which to retrieve the Earnings Calendar
#' @param to last Date for which to retrieve the Earnings Calendar
#' @return a \code{data.frame} with columns \dQuote{Date}, \dQuote{Time}, 
#'   \dQuote{Symbol}, \dQuote{Company}
#' @author Garrett See
#' @references \url{http://biz.yahoo.com/research/earncal/today.html}
#' @note ALPHA CODE!!! Subject to change.
#' @seealso \code{\link{getEconomicCalendar}}, 
#'   \code{\link{getCalendarByDay}}
#' @examples
#' \dontrun{
#' ## fetch the Earnings Calendar from yahoo for today
#' .getEarningsCalendar()
#' ## from a single date in the past or future
#' .getEarningsCalendar("2012-01-04") 
#'
#' ## fetch the Earnings Calendar from yahoo for a range of dates
#' getEarningsCalendar(from="2012-01-04", to="2012-01-06")
#' ## can also use like .getEarningsCalendar and only use a single Date
#' getEarningsCalendar("2012-01-04") # same as .getEarningsCalendar('2012-01-04')
#' }
#' @export
#' @rdname getEarningsCalendar
.getEarningsCalendar <- function(Date=Sys.Date()) {
  require("XML")
  Date <- as.Date(Date)
  rt <- try(readHTMLTable(paste0("http://biz.yahoo.com/research/earncal/",
                                 format(Date, "%Y%m%d"), ".html"), 
                          stringsAsFactors=FALSE), silent=TRUE)
  if (inherits(rt, 'try-error')) { return(NULL) }
  dat <- rt[[tail(grep("Earnings", rt), 1)]]
  #dat <- rt[[which.max(sapply(rt, nrow))]]
  colnames(dat) <- make.names(dat[2, ])
  dat <- dat[-c(1, 2), ]
  out <- na.omit(cbind(data.frame(Date=rep(Date, nrow(dat))), 
                       dat[, c("Time", "Symbol", "Company")]))
  out
}

#' @export
#' @rdname getEarningsCalendar
getEarningsCalendar <- function(from, to) {
  getCalendarByDay(".getEarningsCalendar", from=from, to=to)
}


#' Get Calendar of Mergers
#' 
#' Create a \code{data.frame} from yahoo's calender of mergers
#' 
#' \code{.getMergersCalendar} will usually be called by 
#' \code{\link{getCalendarByMonth}}, but can also be called directly.  It 
#' is used to get the Mergers Calendar for a single month
#' 
#' \code{getMergersCalendar} is a wrapper to get the Mergers Calendar over 
#' many months.
#' @param YM a six character string with the first 4 characters representing the
#'   year and the last 2 characters representing the month of the year (01-12).
#' @param from Date that is in the earliest month to retrieve.
#' @param to Date that is in the last month to retrieve.
#' @return a \code{data.frame} with Dates and information about Mergers and 
#'   Acquisitions that occurred during the requested timeframe.
#' @author Garrett See
#' @references http://biz.yahoo.com/me
#' @note ALPHA CODE!!! Subject to change.
#' @seealso \code{\link{getEconomicCalendar}}, 
#' \code{\link{getEarningsCalendar}},
#' \code{\link{getCalendarByMonth}}
#' @examples
#' \dontrun{
#' .getMergersCalendar("201202")
#' }
#' @export
#' @rdname getMergersCalendar
.getMergersCalendar <- function(YM=format(Sys.Date(), "%Y%m")) {
  require(XML)
  stopifnot(length(YM) == 1)
  if (is.timeBased(YM) || nchar(YM) == 10) {
    YM <- format(as.Date(YM), "%Y%m")
  } else if (nchar(YM) == 5) {
    YM <- paste0(substr(YM, 1, 4), 0, substr(YM, 5, 5))
  } else if (nchar(YM) == 7 && length(grep("-", YM) == 1)) {
    YM <- sub("-", "", YM)
  }
  if (nchar(YM) != 6) stop("'YM' should be 6 digits or a Date")
  Y <- substr(YM, 1, 4)
  # there is a different URL for the current month than for other months
  URL <- if (identical(format(Sys.Date(), "%Y%m"), YM)) {
    "http://biz.yahoo.com/me/"
  } else paste0("http://biz.yahoo.com/me/", YM, ".html")    
  rt <- try(readHTMLTable(URL, stringsAsFactors=FALSE), silent=TRUE)
  if (inherits(rt, 'try-error')) return(NULL)
  dat <- rt[[which.max(sapply(rt, nrow))]]
  colnames(dat) <- make.names(dat[1, ])
  dat <- dat[-1, -1]
  #read.zoo(dat, index.column=1:2
  cal <- data.frame(Date=as.Date(paste(Y, dat[, 1]), format="%Y %b %d"), 
                    dat[, -1])
  cal
}

#' @export
#' @rdname getMergersCalendar
getMergersCalendar <- function(from, to) {
  getCalendarByMonth(".getMergersCalendar", from=from, to=to)
}



#Date <- "2012-06-11"
#surprises by day
#http://biz.yahoo.com/z/20120611.html
#http://biz.yahoo.com/z/20120618.html
#http://biz.yahoo.com/z/20120615.html # no upside

# .getSurprisesCalendar <- function(Date=Sys.Date()) {
#   #TODO: The data.frame you get back needs to be split up into a list of 3
#   # Upside Surprises, Met Expectations, and Downside Surprises.
#   Date <- as.Date(Date)
#   URL <- if (identical(Sys.Date(), Date)) {
#     "http://biz.yahoo.com/z/extreme.html" 
#   } else paste0("http://biz.yahoo.com/z/", format(Date, "%Y%m%d"), ".html")
#   rt <- readHTMLTable(URL, stringsAsFactors=FALSE)
#   
#   dat <- rt[[tail(grep("Earnings", rt), 1)]]
#   #dat <- rt[[which.max(sapply(rt, nrow))]]
# 
#   upbeg <- grep("Upside\nSurprises", dat[, 1])
#   metbeg <- grep("Met\nExpectations", dat[, 1])
#   dnbeg <- grep("Downside\nSurprises", dat[, 1])
#   
#   lapply(c(upbeg, metbeg, dnbeg, nrow(dat)), function(i) {
#     
#   })
#   
#   colnames(dat) <- make.names(dat[2, ])
#   dat <- dat[-c(1, 2), ]
#   dat
#   out <- na.omit(cbind(data.frame(Date=rep(Date, nrow(dat))), 
#                        dat[, c("Time", "Symbol", "Company")]))
#   out
# }

# 
# #surprises by day
# http://biz.yahoo.com/z/extreme.html 
# http://biz.yahoo.com/z/20120611.html
# 
# #splits
# http://biz.yahoo.com/c/s.html
# http://biz.yahoo.com/c/11/s12.html #201112
# http://biz.yahoo.com/c/12/s5.html  #201205



#' Get dividends calendar from earnings.com
#' 
#' Get information about dividends of stocks that go (went) ex-dividend on a
#' given Date or range of dates.
#' 
#' \code{.getDividendsCalendar} will usually be called by 
#' \code{\link{getCalendarByDay}}, but it can also be called directly.
#' 
#' \code{getDividendsCalendar} is a wrapper that creates a sequence of dates 
#' between \code{from} and \code{to}, and then applies 
#' \code{.getDividendsCalendar} to each of those dates and, finally, merges the 
#' results together into a single \code{data.frame}.
#' 
#' @param Date a Date, or character string in the format CCYY-MM-DD
#' @param from first Date for which to retrieve the Dividend Calendar
#' @param to last Date for which to retrieve the Dividend Calendar
#' @return a \code{data.frame} with columns \dQuote{SYMBOL}, \dQuote{COMPANY}, 
#'   \dQuote{AMOUNT}, \dQuote{EX-DATE}, \dQuote{PAYABLE}, \dQuote{RECORD}, and
#'   \dQuote{DECLARATION}
#' @author Garrett See
#' @references \url{http://www.earnings.com/dividend.asp?date=&client=cb}
#' @note ALPHA CODE!!! Subject to change.
#' @seealso \code{\link{getEconomicCalendar}}, 
#'   \code{\link{getEarningsCalendar}},
#'   \code{\link{getCalendarByDay}}
#' @examples
#' \dontrun{
#' ## fetch the Dividends Calendar from yahoo for today
#' .getDividendsCalendar()
#' ## fetch the Dividends Calendar for a range of dates
#' getDividendsCalendar(from=Sys.Date(), to=Sys.Date() + 7)
#' }
#' @export
#' @rdname getDividendsCalendar
.getDividendsCalendar <- function(Date=Sys.Date()) {
    stopifnot(require("XML"))
    Date <- as.Date(Date)
    if (Date < Sys.Date() - 90) {
        stop("earnings.com only provides last 3 months of dividend history.")
    }
    URL <- paste0("http://www.earnings.com/dividend.asp?date=", format(Date, "%Y%m%d"), "&client=cb")
    x <- readHTMLTable(URL, stringsAsFactors=FALSE)
    table.loc <- tail(grep("EX-DATE", x), 1)
    if (length(table.loc) == 0L) return(NULL)
    df <- x[[table.loc]]
    header <- df[1, -1]
    df <- df[-1, -1]
    colnames(df) <- header
    df[df == "n/a"] <- NA
    df[, 1] <- gsub("[^A-Za-z0-9\\.-]", "", df[, 1]) #remove non-break spaces
    df <- na.omit(df)
    rownames(df) <- seq_len(NROW(df))
    # Taiwan stocks report both pct and dollar amt.  
    #For simplicity, I'm removing the percentages
    df <- df[!grepl("%", df$AMOUNT), ]
    df$AMOUNT <- gsub("-", "0", df$AMOUNT)
    df$AMOUNT <- as.numeric(gsub("\\$", "", df$AMOUNT))
    #if (gsub("^0", "", format(Date, "%d-%b")) != df[1, "EX-DATE"]) {
    #    stop(paste("No dividend data available for", Date))
    #}
    df
}

#' @export
#' @rdname getDividendsCalendar
getDividendsCalendar <- function(from, to) {
    getCalendarByDay(".getDividendsCalendar", from=from, to=to)
}

