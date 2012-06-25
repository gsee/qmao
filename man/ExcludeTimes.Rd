\name{ExcludeTimes}
\alias{ExcludeTimes}
\title{Exclude a \code{timespan} from an xts object}
\usage{
  ExcludeTimes(x, timespan)
}
\arguments{
  \item{x}{xts object}

  \item{timespan}{xts-style time-of-day subset}
}
\value{
  xts object that does not include data during the
  specified \code{timespan}
}
\description{
  Exclude a \code{timespan} from an xts object
}
\examples{
.seq <- seq_len(864000)
a <- xts(.seq, Sys.time() + .seq)
b <-  ExcludeTimes(a, "T01:00/T23:00")
diff(b)[diff(b) != 1]

b <-  ExcludeTimes(a, "T02:00/T01:00")
diff(b)[diff(b) != 1]
}
\author{
  Garrett See
}
\seealso{
  \code{\link{TimeOfDaySubset}}
}

