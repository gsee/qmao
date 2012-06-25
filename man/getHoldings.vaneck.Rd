\name{getHoldings.vaneck}
\alias{getHoldings.vaneck}
\title{get the holdings of a Van Eck ETF}
\usage{
  getHoldings.vaneck(Symbols, env = .GlobalEnv,
    auto.assign = TRUE)
}
\arguments{
  \item{Symbols}{character vector of Van Eck ETF ticker
  symbols}

  \item{env}{environment in which to store holdings data}

  \item{auto.assign}{TRUE/FALSE. If TRUE, the holdings data
  will be stored in an object that has a name that is he
  Symbol appended with \dQuote{.h}}
}
\value{
  either the names of the objects that contain the holdings
  if called with \code{auto.assign=TRUE}, or the holdings
  data.  The returned data is a data.frame with Weights
  (0-100) in the first column, and the names of the stocks
  in the second column. The rownames are the the ticker
  symbols.
}
\description{
  get the holdings of a Van Eck ETF
}
\note{
  This should become part of \code{getHoldings} (or
  getHoldings should become \dQuote{generic}).

  Internally, this uses \code{\link[XML]{readHTMLTable}} to
  read an Excel spreadsheet because \code{readHTMLTable} is
  incredibly easy to implement. However, it is a little bit
  slow.
}
\examples{
\dontrun{
getHoldings.vaneck("GDX")
}
}
\author{
  Garrett See
}
\references{
  \url{http://www.vaneck.com/}
}
\seealso{
  \code{\link{getHoldings}}
}

