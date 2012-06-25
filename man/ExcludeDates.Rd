\name{ExcludeDates}
\alias{ExcludeDates}
\title{Exclude one or more days from an xts object}
\usage{
  ExcludeDates(x, exclude)
}
\arguments{
  \item{x}{An xts object}

  \item{exclude}{character vector of dates}
}
\value{
  an xts object that does not contain any rows from the
  dates specified by \code{exclude}
}
\description{
  remove one or more days of data from an xts object
}
\note{
  be careful passing POSIXct objects to exclude as
  \code{as.Date.POSIXct} has \code{tz="UTC"} hardcoded.
}
\examples{
data(sample_matrix)
x <- as.xts(sample_matrix)
head(ExcludeDates(x, exclude = c("2007-01-05", "2007-01-03")))
}
\author{
  Garrett See
}

