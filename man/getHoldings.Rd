\name{getHoldings}
\alias{getHoldings}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get the holdings of an ETF
}
\description{
get the holdings of an iShares or Sector SPDR ETF.
}
\usage{
getHoldings(symbols, env = .GlobalEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{symbols}{
chr symbols of ETFs
}
  \item{env}{
environment to store the holdings data in
}
}
\details{
Only iShares and the 8 Sector SPDR ETFs are supported. 
}
\value{
Call for side-effect. Holdings data is stored in variables that are \code{symbols} appended with .h
Right now, all that is stored is the symbols and their weights.
}
\references{
Sector SPDRs \url{www.sectorspdr.com}
iShares \url{www.ishares.com}
}
\author{
Garrett See
}
\note{
This should get SPDR data from a csv that can be downloaded at https://www.spdrs.com/product.
Could also be updated to return more info than just weights and symbols.
}

\examples{
\dontrun{
getHoldings('XLF')
getHoldings('IVE')
XLF.h
IVE.h
}
}