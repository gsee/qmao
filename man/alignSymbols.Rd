\name{alignSymbols}
\alias{alignSymbols}
\title{Delete rows that not all Symbols have in common}
\usage{
  alignSymbols(Symbols, env = .GlobalEnv)
}
\arguments{
  \item{Symbols}{character vector of symbol names which
  correspond to xts objects}

  \item{env}{The environment that holds the xts objects}
}
\value{
  used for its side effect.
}
\description{
  Deletes rows that not all Symbols have in common
}
\details{
  alignSymbols will get Symbols from the specified
  environment (.GlobalEnv by default), merge them together,
  and delete all rows that have any NAs.  Then it will
  \sQuote{unmerge} and assign the cleaned data back to the
  xts objects named by \code{Symbols}.
}
\examples{
\dontrun{
Symbols <- c("SPY","DIA","CORN")
getSymbols(Symbols)
length(Cl(SPY));length(Cl(DIA));length(Cl(CORN))
alignSymbols(Symbols)
length(Cl(SPY));length(Cl(DIA));length(Cl(CORN))
}
}
\author{
  Garrett See
}
\seealso{
  \code{\link{gsa}}, \code{\link{PF}}, merge, merge.xts,
  cbind, cbind.xts, assign
}

