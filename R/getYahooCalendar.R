
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
#' \code{\link{.getEarningsCalendar}} is used by \code{getYahooCalendarByDay}, 
#' \code{\link{.getEconomicCalendar}} is used by \code{getYahooCalendarByWeek},
#' and 
#' \code{\link{.getMergersCalendar}} is used by \code{getYahooCalendarsByMonth}
#' 
#' Hopefully new ones will be written soon; Splits are in the works (which will
#' use \code{getYahooCalendarByMonth} as well as 
#' Earnings Surprises (which will use getYahooCalendarByDay.)
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
#' getYahooCalendarByDay('.getEconomicCalendar')
#' getYahooCalendarByWeek('.getEarningsCalendar')
#' getYahooCalendarByMonth('.getMergersCalendar', '2012-06-01')
#' }
#' @rdname getYahooCalendar
getYahooCalendarByDay <- function(FUN, from, to) {
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
    out <- lapply(s, FUN)
    return(do.call(rbind, out))
  }
}

#' @rdname getYahooCalendar
getYahooCalendarByWeek <- function(FUN, from, to) {
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

#' @rdname getYahooCalendar
getYahooCalendarByMonth <- function(FUN, from, to) {
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

#' Get the Economic Calender from yahoo.
#' 
#' Download the Briefing.com economic calendar via yahoo's website, and create 
#' a data.frame containing information about previous and scheduled 
#' realeases of economic economic indicators. The returned \code{data.frame} 
#' will have a \code{Time} column that contains an intraday timestamp with 
#' a time zone of \code{America/New_York}.
#' 
#' \code{.getEconomicCalendar} will retrieve the Economic Calendar from Yahoo
#' for a single week.  This function is intended to be called by 
#' \code{getYahooCalendar}, but it can also be called directly.
#' 
#' \code{getEconomicCalendar} is a wrapper that accepts both a \code{from} and 
#' \code{to} argument.  It will use \code{\link{getYahooCalendarByWeek}} to 
#' make repeated calls to \code{.getEconomicCalendar} allowing for the retrieval 
#' of an Economic Calendar over a much longer timespan.
#' 
#' \code{from} and \code{to} are used to pick the first and last \emph{week} to
#' download.  If \code{from} is a Date that is a Wednesday, the first data will
#' be from previous Monday.  Likewise, if \code{to} is a Date that is a 
#' Wednesday, the last data will be from the Friday of that week.
#' 
#' @param YW a six character string with the first 4 characters representing the
#'   year and the last 2 characters representing the week of the year. For
#'   example, \dQuote{201217} would be the 17th week of 2012.
#' @param from Date that is in the earliest week to retrieve.
#' @param to Date that is in the last week to retrieve.
#' @return a data.frame containing the economic calendar for the week
#'   specified by \code{YW}, or for all weeks between and including \code{from} 
#'   and \code{to}. It will have columns:
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
#'   \code{\link{getYahooCalendarByWeek}}
#' @examples
#' \dontrun{
#' .getEconomicCalendar()
#' .getEconomicCalendar(201117)
#' .getEconomicCalendar("201117") #same
#' getEconomicCalendar(from='2012-06-04', to='2012-06-10') #only goes through Friday 2012-06-08
#' getEconomicCalendar(from='2012-06-04', to='2012-06-11') #goes through Friday 2012-06-15
#' }
#' @export
#' @rdname getEconomicCalendar
.getEconomicCalendar <- function(YW=format(Sys.Date(), "%Y%W")) {
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
getEconomicCalendar <- function(from, to) {
  getYahooCalendarByWeek(".getEconomicCalendar", from=from, to=to)
}


#' Get the earnings calendar from yahoo
#' 
#' Get a data.frame of all the stocks that announce(d) earnings on a given Date.
#' 
#' \code{.getEarningsCalendar} will usually be called by 
#' \code{\link{getYahooCalendarByDay}}, but it can also be called directly.
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
#'   \code{\link{getYahooCalendarByDay}}
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
  getYahooCalendarByDay(".getEarningsCalendar", from=from, to=to)
}



#' Get Calendar of Mergers
#' 
#' Create a \code{data.frame} from yahoo's calender of mergers
#' 
#' \code{.getMergersCalendar} will usually be called by 
#' \code{\link{getYahooCalendarByMonth}}, but can also be called directly.  It 
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
#' \code{\link{getYahooCalendarByMonth}}
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
  getYahooCalendarByMonth(".getMergersCalendar", from=from, to=to)
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



