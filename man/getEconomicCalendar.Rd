\name{.getEconomicCalendar}
\alias{getEconomicCalendar}
\alias{.getEconomicCalendar}
\title{Get the Economic Calender from yahoo.}
\usage{
  .getEconomicCalendar(YW = format(Sys.Date(), "\%Y\%W"))

  getEconomicCalendar(from, to)
}
\arguments{
  \item{YW}{a six character string with the first 4
  characters representing the year and the last 2
  characters representing the week of the year. For
  example, \dQuote{201217} would be the 17th week of 2012.}

  \item{from}{Date that is in the earliest week to
  retrieve.}

  \item{to}{Date that is in the last week to retrieve.}
}
\value{
  a data.frame containing the economic calendar for the
  week specified by \code{YW}. It will have columns
  \item{Time}{POSIXct} object with \code{America/New_York}
  time zone \item{Statistic}{Description of the data being
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
  Download the Briefing.com economic calendar via yahoo's
  website, and create a data.frame containing information
  about previous and scheduled realeases of economic
  economic indicators. The returned \code{data.frame} will
  have a \code{Time} column that contains an intraday
  timestamp with a time zone of \code{America/New_York}.
}
\details{
  \code{.getEconomicCalendar} will retrieve the Economic
  Calendar from Yahoo for a single week.  This function is
  intended to be called by \code{getYahooCalendar}, but it
  can also be called directly.

  \code{getEconomicCalendar} is a wrapper that accepts both
  a \code{from} and \code{to} argument.  It will use
  \code{\link{getYahooCalendarByWeek}} to make repeated
  calls to \code{.getEconomicCalendar} allowing for the
  retrieval of an Economic Calendar over a much longer
  timespan.

  \code{from} and \code{to} are used to pick the first and
  last \emph{week} to download.  If \code{from} is a Date
  that is a Wednesday, the first data will be from previous
  Monday.  Likewise, if \code{to} is a Date that is a
  Wednesday, the last data will be from the Friday of that
  week.
}
\note{
  ALPHA CODE!!! Subject to change.
}
\examples{
\dontrun{
.getEconomicCalendar()
.getEconomicCalendar(201117)
.getEconomicCalendar("201117") #same
getEconomicCalendar(from='2012-06-04', to='2012-06-10') #only goes through Friday 2012-06-08
getEconomicCalendar(from='2012-06-04', to='2012-06-11') #goes through Friday 2012-06-15
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
  \code{\link{getYahooCalendarByWeek}}
}

