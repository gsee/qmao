\name{.getMergersCalendar}
\alias{.getMergersCalendar}
\alias{getMergersCalendar}
\title{Get Calendar of Mergers}
\usage{
  .getMergersCalendar(YM = format(Sys.Date(), "\%Y\%m"))

  getMergersCalendar(from, to)
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
  Mergers and Acquisitions that occurred during the
  requested timeframe.
}
\description{
  Create a \code{data.frame} from yahoo's calender of
  mergers
}
\details{
  \code{.getMergersCalendar} will usually be called by
  \code{\link{getCalendarByMonth}}, but can also be called
  directly.  It is used to get the Mergers Calendar for a
  single month

  \code{getMergersCalendar} is a wrapper to get the Mergers
  Calendar over many months.
}
\note{
  ALPHA CODE!!! Subject to change.
}
\examples{
\dontrun{
.getMergersCalendar("201202")
}
}
\author{
  Garrett See
}
\references{
  http://biz.yahoo.com/me
}
\seealso{
  \code{\link{getEconomicCalendar}},
  \code{\link{getEarningsCalendar}},
  \code{\link{getCalendarByMonth}}
}

