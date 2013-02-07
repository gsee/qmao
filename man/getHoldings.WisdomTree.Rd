\name{getHoldings.WisdomTree}
\alias{getHoldings.WisdomTree}
\title{Get the holdings of Wisdom Tree ETFs}
\usage{
  getHoldings.WisdomTree(Symbols, env = .GlobalEnv,
    auto.assign = TRUE)
}
\arguments{
  \item{Symbols}{chr Symbols of ETFs (if not supplied, the
  symbols of all Wisdom Tree ETFs will be used.)}

  \item{env}{environment in which to store the holdings
  data}

  \item{auto.assign}{assign data?}
}
\value{
  An object classed as \dQuote{holdings} will be created
  that is a \code{data.frame} with columns for holdings'
  weights, names, and symbols. If called with
  \code{auto.assign=TRUE}, it will be assigned in
  \code{env} with names that are \code{Symbols} appended
  with \dQuote{.h}.  Otherwise, the \dQuote{holdings} will
  be returned and not stored.
}
\description{
  Get the names and weights of holdings of Wisdom Tree
  ETFs.
}
\details{
  This function is usually called by
  \code{\link{getHoldings}}, but it can also be called
  directly
}
\note{
  This does not support Alternative Investment ETFs which,
  as of this writing (2012-07), are \dQuote{WDTI} and
  \dQuote{RRF}.  Wisdom Tree provides these holdings data
  in the form of Excel spreadsheets and a POST is required
  to download them.

  WisdomTree provides more columns than this function
  returns, but some of their files have multiple sections
  with differing numbers of columns.  Also, they have
  different numbers of columns for different ETFs.  So, at
  least for now, only 3 columns are returned with names
  "Weight", "Name", and "Symbol"
}
\examples{
\dontrun{
getHoldings.WisdomTree('EU')
EU.h
getHoldings.WisdomTree('ICN', auto.assign=FALSE)
getHoldings.WisdomTree('DTN', auto.assign=FALSE)
}
}
\author{
  Garrett See
}
\references{
  \url{http://www.wisdomtree.com/etfs/}
}
\seealso{
  \code{\link{getHoldings}}
}

