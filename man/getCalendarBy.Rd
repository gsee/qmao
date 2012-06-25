\name{getCalendarByDay}
\alias{getCalendarByDay}
\alias{getCalendarByMonth}
\alias{getCalendarByWeek}
\title{Get a calendar over more than one time period from yahoo's website}
\usage{
  getCalendarByDay(FUN, from, to)

  getCalendarByWeek(FUN, from, to)

  getCalendarByMonth(FUN, from, to)
}
\arguments{
  \item{FUN}{a function to be applied to all time periods
  between \code{from} and \code{to}.}

  \item{from}{first time period contains this Date}

  \item{to}{last time period contains this Date}
}
\description{
  Given a \code{from} and \code{to} argument, download a
  calendar from every time period between those Dates, and
  merge them together into a \code{data.frame}.
}
\details{
  These functions take a function that is designed to only
  download a calendar over a single timeperiod.  They then
  apply that function over as many timeperiods as necessary
  to create a complete calendar from \code{from} to
  \code{to}.

  Currently, \code{\link{.getEarningsCalendar}} and
  \code{\link{.getEconomicCalendarBriefing}} are used by
  \code{getCalendarByDay},
  \code{\link{.getEconomicCalendarYahoo}} is used by
  \code{getCalendarByWeek}, and
  \code{\link{.getMergersCalendar}} is used by
  \code{getCalendarsByMonth}

  Hopefully new ones will be written soon; Splits are in
  the works (which will use \code{getCalendarByMonth} as
  well as Earnings Surprises (which will use
  getCalendarByDay.)
}
\note{
  ALPHA CODE: Subject to change!
}
\examples{
\dontrun{
getCalendarByDay('.getEconomicCalendar')
getCalendarByWeek('.getEarningsCalendar')
getCalendarByMonth('.getMergersCalendar', '2012-06-01')
}
}
\author{
  Garrett See
}
\references{
  \url{http://biz.yahoo.com/research/earncal/today.html}

  \url{http://biz.yahoo.com/c/e.html}
}
\seealso{
  \code{\link{getEconomicCalendar}},
  \code{\link{getEarningsCalendar}}
}

