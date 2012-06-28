\name{getHoldings.powershares}
\alias{getHoldings.powershares}
\title{Get the names and weights of the holdings of PowerShares ETFs}
\usage{
  getHoldings.powershares(Symbols, env = .GlobalEnv,
    auto.assign = TRUE)
}
\arguments{
  \item{Symbols}{character vector of PowerShares ETF
  symbols}

  \item{env}{where to store holdings}

  \item{auto.assign}{should the results be assigned in
  \code{env}?}
}
\value{
  if \code{auto.assign} is TRUE, holdings will be assigned
  as the ETF symbols appended with \dQuote{.h}, and the
  names of those objects will be returned. Otherwise, if
  \code{Symbols} is only one symbol, its holdings will be
  returned.  If \code{Symbols} is of length greater than
  one, a list will be returned where each element is the
  holdings of a different ETF.
}
\description{
  Download the holdings of PowerShares ETFs and return a
  data.frame containing the weights and names of the
  holdings (among other things).
}
\details{
  First, this will download a list of PowerShares ETFs.  If
  \code{Symbols} is missing, the holdings of all
  PowerShares ETFs will be retrieved.  Otherwise, only the
  \code{Symbols} that are in the list of PowerShares ETFs
  will be used.  Powershares does NOT provide holdings
  details about \sQuote{Commodity & Currency} ETFs, and
  they provide different columns depending on the type of
  ETF (i.e. equity, preferred stock, fixed income, etc.).
  Although different columns may be returned depending on
  which ETF you request, the first two columns will always
  be the same: Weight (1-100), and Name.

  This will return a data.frame with a row for each holding
  of the ETF.  If the holdings are stocks, the rownames
  will be the stocks' ticker symbols. Otherwise, if there
  is a column called \dQuote{SecurityNum} (which all of
  them presently have), the rownames will become
  \code{make.names(SecurityNum, unique=TRUE)}.  Finally, if
  there is no \dQuote{SecurityNum} column the rows will not
  be named.
}
\examples{
\dontrun{
getHoldings.powershares(c("QQQ", "PGX"))
head(QQQ.h); head(PGX.h)
}
}
\author{
  Garrett See with help from ttmaccer at
  \url{http://stackoverflow.com/a/11004901}
}
\references{
  \url{http://www.invescopowershares.com}
}
\seealso{
  \code{qmao:::getHoldings},
  \code{gsee:::getHoldings.iShares.AsOf}
  \code{gsee:::getHoldings.SPDR}
  \code{gsee:::getHoldings.vaneck}
}

