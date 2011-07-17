\name{alignSymbols}
\alias{alignSymbols}
\title{
Delete rows that not all symbols have in common
}
\description{
Deletes rows that not all symbols have in common
}
\usage{
alignSymbols(symbols, env = .GlobalEnv)
}

\arguments{
  \item{symbols}{
character vector of symbol names which correspond to xts objects
}
  \item{env}{
The environment that holds the xts objects
}
}
\details{
alignSymbols will get symbols from the specified environment (.GlobalEnv by default),
merge them together, and delete all rows that have any NAs.  Then it will \sQuote{unmerge} and assign the cleaned data back to the xts objects named by \code{symbols}.
}
\value{
used for its side effect.
}
\author{
Garrett See
}
\seealso{
merge, merge.xts, cbind, cbind.xts, assign
}
\examples{
\dontrun{
symbols <- c("SPY","DIA","CORN")
getSymbols(symbols)
length(Cl(SPY));length(Cl(DIA));length(Cl(CORN))
alignSymbols(symbols)
length(Cl(SPY));length(Cl(DIA));length(Cl(CORN))
}
}
