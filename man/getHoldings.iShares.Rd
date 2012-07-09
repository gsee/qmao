\name{getHoldings.iShares}
\alias{getHoldings.iShares}
\title{Get the holdings of iShares ETFs}
\usage{
  getHoldings.iShares(Symbols, env = .GlobalEnv,
    auto.assign = TRUE)
}
\arguments{
  \item{Symbols}{chr Symbols of ETFs (if not supplied, the
  symbols of all iShares will be used, which could take a
  long time since there are over 200 of them.)}

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
  Get the names and weights of iShares ETFs.
}
\details{
  This function is usually called by
  \code{\link{getHoldings}}, but it can also be called
  directly

  Some iShares ETFs have more than one holding listed with
  the same Symbol. In the case that Symbols are duplicated,
  a \dQuote{duplicates} attribute will be added to the
  returned object.  Also, since rownames cannot be
  duplicated, they will be made unique with
  \code{\link{make.unique}}
}
\examples{
\dontrun{
getHoldings.iShares('IVE')
IVE.h
}
}
\author{
  Garrett See
}
\references{
  \url{www.ishares.com}#'
}
\seealso{
  \code{\link{getHoldings}}
}

