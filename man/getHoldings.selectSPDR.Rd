\name{getHoldings.selectSPDR}
\alias{getHoldings.selectSPDR}
\title{Get names and weights of the holdings of Select Sector SPDR ETFs}
\usage{
  getHoldings.selectSPDR(Symbols, env = .GlobalEnv,
    auto.assign = TRUE)
}
\arguments{
  \item{Symbols}{character vector of Select Sector SPDR ETF
  symbols.  If not provided, all 9 will be used.}

  \item{env}{where to store holdings (only used if
  \code{auto.assign} is \code{TRUE}}

  \item{auto.assign}{should the results be assigned in
  \code{env}?}
}
\value{
  For each of the \code{Symbols}, an object classed as
  \dQuote{weights}.  If \code{auto.assign} is TRUE,
  holdings will be assigned as the ETF symbols appended
  with \dQuote{.h}, and the names of those objects will be
  returned. Otherwise, if \code{Symbols} is only one
  symbol, its holdings will be returned.  If \code{Symbols}
  is of length greater than one, a list will be returned
  where each element is the holdings of a different ETF.
  If there are no holdings found for a Symbol (most likely
  because it is not a SPDR ETF), and \code{auto.assign} is
  TRUE, nothing will be assigned for that Symbol, but if
  \code{auto.assign} is FALSE, the returned list will have
  \code{NULL} for the element corresponding to that Symbol.
}
\description{
  This function only works with the 9 Select Sector SPDRs:
  \sQuote{XLY}, \sQuote{XLP}, \sQuote{XLE}, \sQuote{XLF},
  \sQuote{XLV}, \sQuote{XLI}, \sQuote{XLB}, \sQuote{XLK},
  \sQuote{XLU}.
}
\note{
  On non-Unix-like platforms, this function is called by
  \code{\link{getHoldings}} since this works cross-platform
  but \code{\link{getHoldings.SPDR}} does not.
}
\examples{
\dontrun{
getHoldings.selectSPDR("XLE")
XLE.h
getHoldings.selectSPDR(auto.assign=FALSE) #list of all of them
}
}
\author{
  Garrett See
}
\references{
  \href{www.sectorspdr.com}{Sector SPDR website}
}
\seealso{
  \code{\link{getHoldings}},
  \code{\link{getHoldings.SPDR}},
  \code{\link{getHoldings.iShares}},
  \code{qmao:::getHoldings.iShares.AsOf}
  \code{\link{getHoldings.vaneck}},
  \code{\link{getHoldings.powershares}} #'
}

