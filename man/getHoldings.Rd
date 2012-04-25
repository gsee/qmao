\name{getHoldings}
\alias{getHoldings}
\title{get the holdings of an ETF}
\usage{
  getHoldings(Symbols, env = .GlobalEnv,
    auto.assign = TRUE)
}
\arguments{
  \item{Symbols}{chr Symbols of ETFs (if not supplied the
  Symbols of the nine Sector SPDRs will be used)}

  \item{env}{environment to store the holdings data in}

  \item{auto.assign}{assign data?}
}
\value{
  An object classed as \dQuote{weights} will be created
  that is a data.frame with columns for holdings' weights
  and names.  If called with \code{auto.assign=TRUE}, it
  will be assigned in \code{env} with names that are
  \code{Symbols} appended with \dQuote{.h}.  Otherwise, the
  \dQuote{weights} will be returned and not stored.
}
\description{
  get the holdings of an iShares or Sector SPDR ETF.
}
\details{
  Only iShares and the 8 Sector SPDR ETFs are supported.

  Some iShares ETFs have more than one holding listed with
  the same Symbol. In the case that Symbols are duplicated,
  a \dQuote{duplicates} attribute will be added to the
  returned object.  Also, since rownames cannot be
  duplicated, they will be made unique with
  \code{\link{make.unique}}
}
\note{
  This should get SPDR data from a csv that can be
  downloaded at https://www.spdrs.com/product. Could also
  be updated to return more info than just weights and
  Symbols.
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

