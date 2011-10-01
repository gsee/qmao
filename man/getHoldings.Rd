\name{getHoldings}
\alias{getHoldings}
\title{get the holdings of an ETF}
\usage{
  getHoldings(symbols, env = .GlobalEnv, auto.assign =
  TRUE)
}
\arguments{
  \item{symbols}{chr symbols of ETFs}

  \item{env}{environment to store the holdings data in}

  \item{auto.assign}{assign data?}
}
\value{
  If called with \code{auto.assign=TRUE}, this function is
  called for side-effect. Holdings data are stored in
  variables that are \code{symbols} appended with .h Right
  now, all that is stored is the symbols and their weights.
  If called with \code{auto.assign=FALSE}, Holdings data
  will be returned, and not stored.
}
\description{
  get the holdings of an iShares or Sector SPDR ETF.
}
\details{
  Only iShares and the 8 Sector SPDR ETFs are supported.
}
\note{
  This should get SPDR data from a csv that can be
  downloaded at https://www.spdrs.com/product. Could also
  be updated to return more info than just weights and
  symbols.
}
\examples{
\dontrun{
getHoldings('XLF')
getHoldings('IVE')
XLF.h
IVE.h
}
}
\author{
  Garrett See
}
\references{
  Sector SPDRs \url{www.sectorspdr.com} iShares
  \url{www.ishares.com}
}

