\name{do.rbind}
\alias{do.rbind}
\title{Split into \code{f} [days], apply a function to each
\code{f} [day], and rbind the results together}
\usage{
  do.rbind(x, FUN, f = "days", indexAt = "lastof")
}
\arguments{
  \item{x}{xts object}

  \item{FUN}{function to apply to each period}

  \item{f}{string describing the period to split by}

  \item{indexAt}{Currently only "lastof" (Default) and
  "firstof" are supported. The index will be the last
  (first) time of each period.}
}
\value{
  xts
}
\description{
  this is a simple convenience function for this type of
  usage: \code{do.call(rbind, lapply(split(x, f)), FUN)}
}
\details{
  From the \code{\link[xts]{endpoints}} help page: Valid
  values for the argument \code{f} include: \dQuote{us}
  (microseconds), \dQuote{microseconds}, \dQuote{ms}
  (milliseconds), \dQuote{milliseconds}, \dQuote{secs}
  (seconds), \dQuote{seconds}, \dQuote{mins} (minutes),
  \dQuote{minutes}, \dQuote{hours}, \dQuote{days},
  \dQuote{weeks}, \dQuote{months}, \dQuote{quarters}, and
  \dQuote{years}.
}
\examples{
data(sample_matrix)
x <- as.xts(sample_matrix)
do.rbind(x, NROW, 'weeks')
do.rbind(x, last, 'weeks')
}
\author{
  Garrett See
}

