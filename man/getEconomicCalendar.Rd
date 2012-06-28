\name{.getEconomicCalendarYahoo}
\alias{getEconomicCalendar}
\alias{getEconomicCalendarBriefing}
\alias{.getEconomicCalendarBriefing}
\alias{getEconomicCalendarYahoo}
\alias{.getEconomicCalendarYahoo}
\title{Get the Economic Calender from Briefing.com or Yahoo.}
\usage{
  .getEconomicCalendarYahoo(YW = format(Sys.Date(), "\%Y\%W"))

  .getEconomicCalendarBriefing(Date = Sys.Date())

  getEconomicCalendarYahoo(from, to)

  getEconomicCalendarBriefing(from, to)

  getEconomicCalendar(from, to)
}
\arguments{
  \item{YW}{Used with \code{getEconomicCalendarYahoo}, a
  six character string with the first 4 characters
  representing the year and the last 2 characters
  representing the week of the year. For example,
  \dQuote{201217} would be the 17th week of 2012.}

  \item{Date}{used with \code{getEconomicCalendarBriefing},
  a Date for which to retrieve the Economic Calendar.}

  \item{from}{Date that is in the earliest week to
  retrieve.}

  \item{to}{Date that is in the last week to retrieve.}
}
\value{
  a data.frame containing the economic calendar for the
  time period specified by \code{YW} or \code{Date}, or for
  all time periods between and including \code{from} and
  \code{to}. It will have columns:

  \item{Time}{POSIXct object with \code{America/New_York}
  time zone} \item{Statistic}{Description of the data being
  released} \item{For}{non-standard character string
  indicating the time period over which the
  \code{Statistic} was measured. Could be things like Mar,
  05/26, Q1, April, etc.)} \item{Actual}{Actual observed
  value of the \code{Statistic}}
  \item{Briefing.Forecast}{the value that Briefing.com
  predicted the \code{Statistic} to be}
  \item{Market.Expects}{Consensus forecast}
  \item{Prior}{prior reading}
}
\description{
  Download the Briefing.com economic calendar either
  directly from the briefing.com website, or via yahoo's
  website, and create a data.frame containing information
  about previous and/or scheduled realeases of economic
  economic indicators. The returned \code{data.frame} will
  have a \code{Time} column that contains an intraday
  timestamp with a time zone of \code{America/New_York}.
}
\details{
  \code{.getEconomicCalendarYahoo} will retrieve the
  Economic Calendar from Yahoo for a single week.  Use this
  function if you need to get the economic calendar of
  scheduled releases (i.e. have not yet occurred).  The
  time of day is not always correct for historic releases.
  Therefore, if you are getting Dates from the past, it is
  recommended that you use
  \code{.getEconomicCalendarBriefing} or
  \code{getEconomicCalendarBriefing}

  \code{.getEconomicCalendarBriefing} will retrieve the
  Economic Calendar from Briefing.com for a single day.
  This function does not support Dates that are in the
  future, but is more reliable for past Dates than the data
  obtained via yahoo. (Briefing provides the data to yahoo,
  so I'm not sure why there are discrepancies.)

  \code{getEconomicCalendarYahoo} is a wrapper that accepts
  both a \code{from} and \code{to} argument.  It will use
  \code{\link{getCalendarByWeek}} to make repeated calls to
  \code{.getEconomicCalendarYahoo} allowing for the
  retrieval of an Economic Calendar over a much longer
  timespan.

  for \code{getEconomicCalendarBriefing}, \code{from} and
  \code{to} are the first and last dates that should be
  included in the returned \code{data.frame}.  However, for
  \code{getEconomicCalendarYahoo}, \code{from} and
  \code{to} are used to pick the first and last \emph{week}
  to download.  If \code{from} is a Date that is a
  Wednesday, the first data will be from previous Monday.
  Likewise, if \code{to} is a Date that is a Wednesday, the
  last data will be from the Friday of that week.

  \code{getEconomicCalendar} is a wrapper that will call
  \code{getEconomicCalendarYahoo} if any of the requested
  Dates occur in the future; otherwise, it will call
  \code{getEconomicCalendarBriefing}.
}
\note{
  ALPHA CODE!!! Subject to change.
}
\examples{
\dontrun{
.getEconomicCalendarBriefing()
.getEconomicCalendarBriefing('2012-06-01')

.getEconomicCalendarYahoo()
.getEconomicCalendarYahoo(201117)
.getEconomicCalendarYahoo("201117") #same

getEconomicCalendarYahoo(from='2012-06-04', to='2012-06-10') #only goes through Friday 2012-06-08
getEconomicCalendarYahoo(from='2012-06-04', to='2012-06-11') #goes through Friday 2012-06-15

getEconomicCalendarBriefing(from='2012-06-04', to='2012-06-12') #only goes to 'to' Date

getEconomicCalendar(from=Sys.Date()-5, to=Sys.Date() - 1) #uses Briefing.com because it's Dates from past
getEconomicCalendar(from=Sys.Date(), to=Sys.Date() + 5) #uses Yahoo because it's Dates from future
}
}
\author{
  Garrett See
}
\references{
  \url{http://biz.yahoo.com/c/e.html}
}
\seealso{
  \code{\link{getEarningsCalendar}},
  \code{\link{getCalendarByWeek}}
}

