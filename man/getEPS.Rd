\name{getEPS}
\alias{getEPS}
\title{Get historical Earnings Per Share of a stock}
\usage{
  getEPS(Symbol, freq = c("Q", "A"), src = "google")
}
\arguments{
  \item{Symbol}{A single valid google symbol as a string.
  (may be updated in the future to support vector and
  semi-colon delimited)}

  \item{freq}{Quarterly or Annual (but, only the first
  letter is used, so "Q" and "A" work also)}

  \item{src}{Only 'google' is currently supported.}
}
\value{
  an xts object containing historical Diluted Normalized
  EPS according to google.
}
\description{
  This will use \code{\link[quantmod]{getFinancials}} to
  download the financial statements for \code{Symbol}.
  Then it will extract only the \dQuote{Diluted Normalized
  EPS} row from the Income Statement and convert it to an
  xts object.
}
\examples{
\dontrun{
getEPS("GOOG") # Quarterly by default
getEPS("GOOG","Annual")

# get a single xts object with several stocks' EPS
do.call(cbind, lapply(c("GOOG","MSFT","IBM"), getEPS))
}
}
\seealso{
  \code{\link{getEarnings}} for historic earnings estimates
  and actual earnings from earnings.com.
}

