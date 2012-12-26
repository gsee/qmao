\name{.getSplitsCalendar}
\alias{getSplitsCalendar}
\alias{.getSplitsCalendar}
\title{Get Calendar of Splits}
\usage{
  .getSplitsCalendar(YM = format(Sys.Date(), "\%Y\%m"))

  getSplitsCalendar(from, to)
}
\arguments{
  \item{YM}{a six character string with the first 4
  characters representing the year and the last 2
  characters representing the month of the year (01-12).}

  \item{from}{Date that is in the earliest month to
  retrieve.}

  \item{to}{Date that is in the last month to retrieve.}
}
\value{
  a \code{data.frame} with Dates and information about
  Splits that occurred during the requested timeframe.
}
\description{
  Create a \code{data.frame} from yahoo's calender of
  splits
}
\details{
  \code{.getSplitsCalendar} is used to get the Splits
  Calendar for a single month.  It will usually be called
  by \code{\link{getCalendarByMonth}}, but can also be
  called directly.

  \code{getSplitsCalendar} is a wrapper to get the Splits
  Calendar over many months.
}
\note{
  ALPHA CODE!!! Subject to change.
}
\examples{
\dontrun{
.getSplitsCalendar("201208")
}
}
\author{
  Garrett See
}
\references{
  http://biz.yahoo.com/c/s.html
}
\seealso{
  \code{\link{getMergersCalendar}},
  \code{\link{getEconomicCalendar}},
  \code{\link{getEarningsCalendar}},
  \code{\link{getCalendarByMonth}}
}

