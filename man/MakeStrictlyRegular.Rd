\name{MakeStrictlyRegular}
\alias{MakeStrictlyRegular}
\title{Make an xts object strictly regular.}
\usage{
  MakeStrictlyRegular(x, timespan = "", by = "min",
    tz = "America/Chicago", verbose = TRUE)
}
\arguments{
  \item{x}{xts object}

  \item{timespan}{time-of-day subset string (e.g.
  "T08:30/T15:00"). seealso
  \code{\link[qmao]{TimeOfDaySubset}}}

  \item{by}{increment of the time sequence.  seealso
  \code{\link{seq.POSIXt}}}

  \item{tz}{timezone to use with \code{\link{seq.POSIXt}}}

  \item{verbose}{logical. print to standard output the
  number of rows that were added to make the object
  strictly regular?}
}
\value{
  a strictly regular xts object
}
\description{
  Make an xts object strictly regular.
}
\note{
  ALPHA code; not perfect
}
\examples{
x <- align.time(.xts(1:1000, 60*1:1000))[-c(2, 4, 7, 8), ] # remove some rows at the begining
head(x[paste((start.x <- start(x)), "/")])
x2 <- MakeStrictlyRegular(x)
head(x2[paste(start.x, "/")])
}
\author{
  Garrett See
}

