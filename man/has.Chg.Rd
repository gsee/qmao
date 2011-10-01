\name{has.Chg}
\alias{has.AskSize}
\alias{has.BAM}
\alias{has.BATM}
\alias{has.BidSize}
\alias{has.Chg}
\alias{has.Mid}
\alias{is.BAM}
\alias{is.BATM}
\title{Check for Bid, Ask, Mid, and/or Trade columns in data}
\usage{
  has.Chg(x, which = FALSE)

  has.AskSize(x, which = FALSE)

  has.BidSize(x, which = FALSE)

  is.BAM(x)

  has.BAM(x, which = FALSE)

  has.BATM(x, which = FALSE)

  is.BATM(x)

  has.Mid(x, which = FALSE)
}
\arguments{
  \item{x}{data object}

  \item{which}{display position of match}
}
\value{
  A logical value indicating success or failure by default.

  If which=TRUE, a numeric value representing the column
  position will be returned.

  \code{is.BAM} and \code{is.BATM} return a single value of
  TRUE or FALSE.
}
\description{
  A set of functions to check for appropriate BAM and BATM
  column names within a data object, as well as the
  availability and positions of those columns.
}
\details{
  \code{is.BAM} (and \code{is.BATM}, similarly) will only
  return TRUE if there are columns for Bid, Ask, Mid (and
  Trade for \code{is.BATM}).  Additional columns will not
  affect the value.
}
\author{
  Garrett See (but based on -- and mostly copied from --
  Jeffrey A. Ryan's quantmod)
}
\keyword{utilities}

