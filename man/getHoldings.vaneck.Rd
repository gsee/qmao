\name{getHoldings.vaneck}
\alias{getHoldings.vaneck}
\title{Get the holdings of a Van Eck ETF}
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
  if called with \code{auto.assign=TRUE}, or a list of the
  holdings data.  The returned data will be in objects
  classed as \code{weights} that are data.frames with
  Weights (0-100) in the first column, and the Names of the
  stocks in the second column. The rownames are the the
  ticker symbols.
}
\description{
  Download the names and weights of Van Eck ETFs (Market
  Vector ETFs)
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
  \code{\link{getHoldings}},
  \code{\link{getHoldings.SPDR}},
  \code{\link{getHoldings.iShares}},
  \code{\link{getHoldings.powershares}}
}

