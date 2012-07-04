\name{MakeStrictlyRegular}
\alias{MakeStrictlyRegular}
\title{Make an xts object strictly regular.}
\usage{
  MakeStrictlyRegular(x, timespan = "", by = "min",
    tz = "America/Chicago", verbose = TRUE, na.rm = TRUE,
    maxgap = 5)
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

  \item{na.rm}{Should leading \code{NA}s be removed?}

  \item{maxgap}{see \code{\link[zoo]{na.locf}}}
}
\value{
  a strictly regular xts object
}
\description{
  Fill in gaps in data, by day, using \code{na.locf}
}
\details{
  By default, if there are 5 or more missing observations,
  they will not be filled in.  This can be adjusted with
  \code{maxgap}.  Also, note that this function operates on
  each day separately.  Therefore, missing values at the
  beginning of a day will not be filled-in.
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

