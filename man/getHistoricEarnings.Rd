\name{getHistoricEarnings}
\alias{convertEarningsTime}
\alias{getHistoricEarnings}
\title{download historic earnings and earnings estimates for a given stock}
\usage{
  getHistoricEarnings(Symbol, doFormatTime = TRUE,
    return.tz = "America/Chicago")

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

  \item{x}{character string representing a time or date}

  \item{date.format}{format of the date}

  \item{default.time}{what time to use if \code{x} is only
  a date. \dQuote{AMC} stands for After Market Close and
  will become \dQuote{16:00:00} New York time.
  \dQuote{BMO} stands for Before Market Open and will
  become \dQuote{07:00:00} New York time.}
}
\value{
  \code{getHistoricEarnings} returns a data.frame.
  \code{convertEarningsTime} will return a string
  representing a date and time. in the format "%Y-%m-%d
  %H:%M%:S %Z"
}
\description{
  download historic earnings and earnings estimates for a
  given stock from \url{http://earnings.com}. One of the
  columns of the \code{data.frame} will be the Dates/Times
  of the earnings release.  These Dates/Times may have one
  of a few different formats, most of which are ambiguous.
  \code{convertEarningsTime} will format the Dates/Times in
  an unambiguously; it will be called if
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
getHistoricEarnings('GOOG')
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

