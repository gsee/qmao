\name{.getEarningsCalendar}
\alias{getEarningsCalendar}
\alias{.getEarningsCalendar}
\title{Get the earnings calendar from yahoo}
\usage{
  .getEarningsCalendar(Date = Sys.Date())

  getEarningsCalendar(from, to)
}
\arguments{
  \item{Date}{a Date, or character string in the format
  CCYY-MM-DD}

  \item{from}{first Date for which to retrieve the Earnings
  Calendar}

  \item{to}{last Date for which to retrieve the Earnings
  Calendar}
}
\value{
  a \code{data.frame} with columns \dQuote{Date},
  \dQuote{Time}, \dQuote{Symbol}, \dQuote{Company}
}
\description{
  Get a data.frame of all the stocks that announce(d)
  earnings on a given Date.
}
\details{
  \code{.getEarningsCalendar} will usually be called by
  \code{\link{getYahooCalendarByDay}}, but it can also be
  called directly.

  \code{getEarningsCalendar} is a wrapper that creates a
  sequence of dates between \code{from} and \code{to}, and
  then applies \code{.getEarningsCalendar} to each of those
  dates and, finally, merges the results together into a
  single \code{data.frame}.
}
\note{
  ALPHA CODE!!! Subject to change.
}
\examples{
\dontrun{
## fetch the Earnings Calendar from yahoo for today
.getEarningsCalendar()
## from a single date in the past or future
.getEarningsCalendar("2012-01-04")

## fetch the Earnings Calendar from yahoo for a range of dates
getEarningsCalendar(from="2012-01-04", to="2012-01-06")
## can also use like .getEarningsCalendar and only use a single Date
getEarningsCalendar("2012-01-04") # same as .getEarningsCalendar('2012-01-04')
}
}
\author{
  Garrett See
}
\references{
  \url{http://biz.yahoo.com/research/earncal/today.html}
}
\seealso{
  \code{\link{getEconomicCalendar}},
  \code{\link{getYahooCalendarByDay}}
}

