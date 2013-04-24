\name{getHoldings.iShares.ca}
\alias{getHoldings.iShares.ca}
\title{Get the holdings of Canadian iShares ETFs}
\usage{
  getHoldings.iShares.ca(Symbols, env = .GlobalEnv,
    auto.assign = TRUE)
}
\arguments{
  \item{Symbols}{chr Symbols of ETFs (if not supplied, the
  symbols of all Canadian iShares will be used)}

  \item{env}{environment to store the holdings data in}

  \item{auto.assign}{assign data?}
}
\value{
  An object classed as \dQuote{holdings} will be created
  that is a data.frame with columns for holdings' weights
  and names.  If called with \code{auto.assign=TRUE}, it
  will be assigned in \code{env} with names that are
  \code{Symbols} appended with \dQuote{.h}.  Otherwise, the
  \dQuote{holdings} will be returned and not stored.
}
\description{
  Get the names and weights of Canadian iShares ETFs.
}
\details{
  This function can be called by \code{\link{getHoldings}},
  or it can be called directly

  Some ETFs have more than one holding listed with the same
  Symbol. In the case that Symbols are duplicated, a
  \dQuote{duplicates} attribute will be added to the
  returned object.  Also, since rownames cannot be
  duplicated, they will be made unique with
  \code{\link{make.unique}}
}
\examples{
\dontrun{
getHoldings.iShares.ca("XIU")
XIU.h
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

