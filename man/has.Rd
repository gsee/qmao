\name{has.Chg}
\alias{has.AskSize}
\alias{has.BidSize}
\alias{has.Chg}
\alias{is.BAM}
\alias{is.BATM}
\title{
Check for Bid, Ask, Mid, and/or Trade columns in data
}
\description{
A set of function to check for appropriate BAM and BATM
column names within a data object, as well as the availability
and positions of those columns.
}
\usage{
has.AskSize(x, which=FALSE)
has.BidSize(x, which=FALSE)
has.Chg(x, which=FALSE)
is.BATM(x)
is.BAM(x)
}
\arguments{
  \item{x}{ data object }
  \item{which}{ display position of match }
}
\details{
Mostly used internally by \pkg{quantmod}, they can be useful
for checking whether an object
can be used in OHLC requiring functions like \code{ClOp}, etc.

\code{is.BAM} (and \code{is.BATM}, similarly) will only return TRUE if there are columns
for Bid, Ask, Mid (and Trade for \code{is.BATM}).  Additional columns will not affect
the value.
}
\value{
A logical value indicating success or failure by default.

If which=TRUE, a numeric value representing the column position
will be returned.

\code{is.BAM} and \code{is.BATM} return a single value of TRUE or FALSE.
}
\author{
Garrett See (but based on -- and mostly copied from -- Jeffrey A. Ryan's quantmod)
}
\keyword{ utilities }
