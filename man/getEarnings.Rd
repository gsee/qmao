\name{getEarnings}
\alias{convertEarningsTime}
\alias{getEarnings}
\title{download historic earnings and earnings estimates for a given stock}
\usage{
  getEarnings(Symbol, doFormatTime = TRUE,
    return.tz = "America/Chicago",
    return.class = c("xts", "data.frame"))

  convertEarningsTime(x, date.format = "\%d-\%b-\%y",
    default.time = "AMC", return.tz = "America/Chicago")
}
\arguments{
  \item{Symbol}{character string ticker symbol of stock}

  \item{doFormatTime}{should the values of the Date/Time be
  re-formatted? (TRUE)}

  \item{return.tz}{timezone in which to represent Date/Time
  of earnings release.  ignored if \code{doFormatTime} is
  not \code{TRUE}}

  \item{return.class}{can be one of \dQuote{xts} or
  \dQuote{data.frame}.}

  \item{x}{character string representing a time or date}

  \item{date.format}{format of the date}

  \item{default.time}{what time to use if \code{x} is only
  a date. \dQuote{AMC} stands for After Market Close and
  will become \dQuote{16:00:00} New York time.
  \dQuote{BMO} stands for Before Market Open and will
  become \dQuote{07:00:00} New York time.}
}
\value{
  for \code{getEarnings}, it depends on
  \code{return.class}; If it is \dQuote{xts}, an \code{xts}
  object will be returned that will only contain the
  numeric columns: \dQuote{EPS.ESTIMATE},
  \dQuote{EPS.ACTUAL}, and \dQuote{PREV.YEAR.ACTUAL}. If
  \code{return.class} is \dQuote{data.frame}, a
  \code{data.frame} will be returned that, in addition to
  the columns of the xts, also contain columns
  \dQuote{Symbol}, \dQuote{PERIOD}, \dQuote{EVENT.TITLE},
  and \dQuote{TIME}.

  \code{convertEarningsTime} will return a string
  representing a date and time. in the format
  \dQuote{\%Y-\%m-\%d \%H:\%M\%:S \%Z}
}
\description{
  download historic earnings and earnings estimates for a
  given stock from \url{http://earnings.com}. One of the
  columns of the \code{data.frame} will be the Dates/Times
  of the earnings release.  These Dates/Times may have one
  of a few different formats, most of which are ambiguous.
  \code{convertEarningsTime} will put the Dates/Times in an
  unambiguous format; it will be called if
  \code{getHistoricEarnings} is called with
  \code{doFormatTime = TRUE}.
}
\details{
  If one of the earnings Date/Time values contains
  \dQuote{BMO} or \dQuote{AMC}, it will be substituted with
  \dQuote{07:00:00} and \dQuote{16:15:00}, respectively in
  New York time.
}
\examples{
\dontrun{
getEarnings('GOOG')
getEarnings("GOOG", return.class='data.frame')
getEarnings("GOOG", return.class='data.frame', doFormatTime=FALSE)
}
}
\references{
  \url{http://earnings.com}
}
\seealso{
  \code{\link{getHoldings}},
  \code{\link[quantmod]{getFinancials}},
  \code{\link[quantmod]{getDividends}},
  \code{\link{get_div}}, \code{\link[quantmod]{getSplits}},
  \code{\link{get_spl}},

  \code{\link[base]{strftime}} for formats
}

