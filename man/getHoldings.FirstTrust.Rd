\name{getHoldings.FirstTrust}
\alias{getHoldings.FirstTrust}
\title{Get the holdings of First Trust ETFs}
\usage{
  getHoldings.FirstTrust(Symbols, env = .GlobalEnv,
    auto.assign = TRUE)
}
\arguments{
  \item{Symbols}{chr Symbols of ETFs (if not supplied, the
  symbols of all First Trust ETFs will be used.)}

  \item{env}{environment in which to store the holdings
  data}

  \item{auto.assign}{assign data?}
}
\value{
  An object classed as \dQuote{holdings} will be created
  that is a \code{data.frame} with columns for holdings'
  weights and names.  If called with
  \code{auto.assign=TRUE}, it will be assigned in
  \code{env} with names that are \code{Symbols} appended
  with \dQuote{.h}.  Otherwise, the \dQuote{holdings} will
  be returned and not stored.
}
\description{
  Get the names and weights of holdings of First Trust
  ETFs.
}
\details{
  This function is usually called by
  \code{\link{getHoldings}}, but it can also be called
  directly
}
\examples{
\dontrun{
getHoldings.FirstTrust('FDN')
FDN.h
}
}
\author{
  Garrett See
}
\references{
  \url{http://www.ftportfolios.com/index.aspx}
}
\seealso{
  \code{\link{getHoldings}}
}

