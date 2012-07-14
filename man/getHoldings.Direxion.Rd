\name{getHoldings.Direxion}
\alias{getHoldings.Direxion}
\title{Get the holdings of Direxion ETFs}
\usage{
  getHoldings.Direxion(Symbols, env = .GlobalEnv,
    auto.assign = TRUE)
}
\arguments{
  \item{Symbols}{chr Symbols of ETFs (if not supplied, the
  symbols of all Direxion ETFs will be used.)}

  \item{env}{environment in which to store the holdings
  data}

  \item{auto.assign}{assign data?}
}
\value{
  An object classed as \dQuote{weights} will be created
  that is a \code{data.frame} with columns for holdings'
  weights and names.  If called with
  \code{auto.assign=TRUE}, it will be assigned in
  \code{env} with names that are \code{Symbols} appended
  with \dQuote{.h}.  Otherwise, the \dQuote{weights} will
  be returned and not stored.
}
\description{
  Get the names and weights of Direxion ETFs holdings.
}
\details{
  This function is usually called by
  \code{\link{getHoldings}}, but it can also be called
  directly.
}
\examples{
\dontrun{
getHoldings.Direxion("FAS")
FAS.h
}
}
\author{
  Garrett See
}
\references{
  \url{http://www.direxionshares.com/etfs}
}
\seealso{
  \code{\link{getHoldings}}, \code{qmao:::DirexionSymbols}
}

