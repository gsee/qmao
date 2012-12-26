\name{.getDividendsCalendar}
\alias{getDividendsCalendar}
\alias{.getDividendsCalendar}
\title{Get dividends calendar from earnings.com}
\usage{
  .getDividendsCalendar(Date = Sys.Date())

  getDividendsCalendar(from, to)
}
\arguments{
  \item{Date}{a Date, or character string in the format
  CCYY-MM-DD}

  \item{from}{first Date for which to retrieve the Dividend
  Calendar}

  \item{to}{last Date for which to retrieve the Dividend
  Calendar}
}
\value{
  a \code{data.frame} with columns \dQuote{SYMBOL},
  \dQuote{COMPANY}, \dQuote{AMOUNT}, \dQuote{EX-DATE},
  \dQuote{PAYABLE}, \dQuote{RECORD}, and
  \dQuote{DECLARATION}
}
\description{
  Get information about dividends of stocks that go (went)
  ex-dividend on a given Date or range of dates.
}
\details{
  \code{.getDividendsCalendar} will usually be called by
  \code{\link{getCalendarByDay}}, but it can also be called
  directly.

  \code{getDividendsCalendar} is a wrapper that creates a
  sequence of dates between \code{from} and \code{to}, and
  then applies \code{.getDividendsCalendar} to each of
  those dates and, finally, merges the results together
  into a single \code{data.frame}.
}
\note{
  ALPHA CODE!!! Subject to change.
}
\examples{
\dontrun{
## fetch the Dividends Calendar from yahoo for today
.getDividendsCalendar()
## fetch the Dividends Calendar for a range of dates
getDividendsCalendar(from=Sys.Date(), to=Sys.Date() + 7)
}
}
\author{
  Garrett See
}
\references{
  \url{http://www.earnings.com/dividend.asp?date=&client=cb}
}
\seealso{
  \code{\link{getEconomicCalendar}},
  \code{\link{getEarningsCalendar}},
  \code{\link{getCalendarByDay}}
}

