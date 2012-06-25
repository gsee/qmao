\name{TimeOfDaySubset}
\alias{TimeOfDaySubset}
\title{Subset by time-of-day}
\usage{
  TimeOfDaySubset(x, timespan)
}
\arguments{
  \item{x}{xts data object}

  \item{timespan}{string (e.g. \dQuote{T08:30/T15:00} or
  \dQuote{"T17:00/T15:15"})}
}
\value{
  an extraction of the original xts object that includes
  either times betweeen the first and last time of
  \code{timespan} if the first time comes earlier in the
  day than the second, or times that are not between the
  last and first time if the the last time comes earlier in
  the day than the first time.
}
\description{
  If the first time in the string is later in the day than
  the second time, the data between those times will be
  removed.  Otherwise, \code{x[timespan]} will be returned.
}
\examples{
x <- xts(1:10080, Sys.time() + 60 * 1:10080)
NROW(aggregate(x, format(index(x), "\%H"), length)) # number of hours that have data
x1 <- TimeOfDaySubset(x, "T08:30/T14:59.999")
NROW(aggregate(x1, format(index(x1), "\%H"), length))
x2 <- TimeOfDaySubset(x, "T18:00/T13:00.000")
NROW(aggregate(x2, format(index(x2), "\%H"), length))
}
\author{
  Garrett See
}
\seealso{
  \code{\link{ExcludeTimes}}
}

