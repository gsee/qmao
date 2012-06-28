\name{getHoldings.SPDR}
\alias{getHoldings.SPDR}
\title{Get names and weights of the holdings of SPDR ETFs}
\usage{
  getHoldings.SPDR(Symbols, env = .GlobalEnv,
    auto.assign = TRUE)
}
\arguments{
  \item{Symbols}{character vector of SPDR ETF symbols.
  Presently, if no \code{Symbols} are provided, all SPDR
  symbols will be used.  However, in the future it may be
  changed to require that \code{Symbols} is not
  \code{missing}.}

  \item{env}{where to store holdings (only used if
  \code{auto.assign} is \code{TRUE}}

  \item{auto.assign}{should the results be assigned in
  \code{env}?}
}
\value{
  if \code{auto.assign} is TRUE, holdings will be assigned
  as the ETF symbols appended with \dQuote{.h}, and the
  names of those objects will be returned. Otherwise, if
  \code{Symbols} is only one symbol, its holdings will be
  returned.  If \code{Symbols} is of length greater than
  one, a list will be returned where each element is the
  holdings of a different ETF.  If there are no holdings
  found for a Symbol (most likely because it is not a SPDR
  ETF), and \code{auto.assign} is TRUE, nothing will be
  assigned for that Symbol, but if \code{auto.assign} is
  FALSE, the returned list will have \code{NULL} for the
  element corresponding to that Symbol.
}
\description{
  On Non-Unix-alike platforms, this will call
  \code{\link{getHoldings.selectSPDR}} which is platform
  independent (but only works for the 9 Select Sector
  SPDRs).
}
\details{
  This function is usually called by
  \code{\link{getHoldings}}, but it can also be called
  directly
}
\note{
  this uses \code{download.file} with \code{method="curl"}
  which is not supported on Windows.
}
\examples{
\dontrun{
getHoldings.SPDR("SPY")
SPY.h
}
}
\author{
  Garrett See
}
\references{
  \url{https://www.spdrs.com/}
}
\seealso{
  \code{\link{getHoldings}},
  \code{\link{getHoldings.iShares}},
  \code{\link{getHoldings.selectSPDR}},
  \code{\link{getHoldings.vaneck}},
  \code{\link{getHoldings.powershares}}
}

