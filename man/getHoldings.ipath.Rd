\name{getHoldings.ipath}
\alias{getHoldings.ipath}
\title{Get the holdings of an iPath ETN}
\usage{
  getHoldings.ipath(Symbols, env = .GlobalEnv,
    auto.assign = TRUE)
}
\arguments{
  \item{Symbols}{character vector of iPath ETN ticker
  symbols. Presently, if \code{Symbols} is \code{missing},
  all the iPath Symbols found by scraping
  \url{http://www.ipathetn.com/us/product_information} will
  be used.  However, in the future this may change to
  require that \code{Symbols} is not \code{missing}}

  \item{env}{environment in which to store holdings data}

  \item{auto.assign}{TRUE/FALSE. If TRUE, the holdings data
  will be stored in an object that has a name that is he
  Symbol appended with \dQuote{.h}}
}
\value{
  either the names of the objects that contain the holdings
  if called with \code{auto.assign=TRUE}, or a list of the
  holdings data.  The returned data will be in objects
  classed as \code{holdings} that are data.frames with
  Weights (0-100) in the first column, and the Names of the
  holdings in the second column. The rownames are the the
  ticker symbols.
}
\description{
  Download the names and weights of holdings of iPath ETNs
}
\note{
  these funds are not included in the masterDATA csv used
  by \code{\link{getHoldings}}.  Therefore,
  \code{getHoldings} \emph{will not} dispatch
  \code{getHoldings.ipath} -- it \emph{must} be called
  directly.
}
\examples{
\dontrun{
getHoldings.ipath("BCM")
}
}
\author{
  Garrett See
}
\references{
  \url{http://www.ipathetn.com/}
}
\seealso{
  \code{\link{getHoldings}},
  \code{\link{getHoldings.SPDR}},
  \code{\link{getHoldings.iShares}},
  \code{\link{getHoldings.powershares}},
  \code{\link{getHoldings.vaneck
}

