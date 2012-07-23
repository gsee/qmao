\name{getHoldings}
\alias{getHoldings}
\title{Get the holdings of an ETF}
\usage{
  getHoldings(Symbols, env = .GlobalEnv,
    auto.assign = TRUE)
}
\arguments{
  \item{Symbols}{chr Symbols of ETFs.  Presently, if not
  supplied the Symbols of the nine Sector SPDRs will be
  used.  However, in the future it may be changed to
  require that \code{Symbols} be provided by the user.}

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
  Returned objects may have more columns depending on which
  \code{getHoldings.*} \dQuote{method} is used.
}
\description{
  Get the names and weights of ETF holdings.
}
\details{
  This function acts like a generic function.  There are
  other functions, such as
  \code{\link{getHoldings.iShares}} and
  \code{\link{getHoldings.selectSPDR}} that act like
  \dQuote{methods}.

  \code{getHoldings} looks up the \code{Symbols} in the CSV
  at
  \url{http://www.masterdata.com/helpfiles/ETF_List_Downloads/AllTypes.csv}
  from
  \url{http://www.masterdata.com/HelpFiles/ETF_List.htm}.
  It uses the \code{Name} field to determine which
  \code{getHoldings.*} function to call.

  Different \code{getHoldings.*} \dQuote{methods} will
  return different columns, but all of them should have the
  Ticker Symbol or some other identifier as the
  \code{rownames}, the first column should be
  \code{Weights} (0-100), and the second column should be
  \code{Name} (a description of the security).  Also, the
  returned holdings data will be a \code{data.frame} with
  class \code{holdings}, or a \code{list} of
  \code{holdings} classed objects.

  Some iShares ETFs have more than one holding listed with
  the same Symbol. In the case that Symbols are duplicated,
  a \dQuote{duplicates} attribute will be added to the
  returned object.  Also, since rownames cannot be
  duplicated, they will be made unique with
  \code{\link{make.unique}}
}
\examples{
\dontrun{
getHoldings('XLB', auto.assign=FALSE) #getHoldings.SPDR or .selectSPDR depending on OS
getHoldings('IVE') #getHoldings.iShares
IVE.h
getHoldings(c("QQQ", "GDX")) #.powershares and .vaneck (Market Vectors)
getHoldings(c("XLY", "IVE", "QQQ", "GDX", "OIH", "XLU", "EPI"))
}
}
\author{
  Garrett See
}
\references{
  \url{http://www.masterdata.com/HelpFiles/ETF_List.htm}
}
\seealso{
  \code{\link{getHoldings.SPDR}},
  \code{\link{getHoldings.selectSPDR}},
  \code{\link{getHoldings.iShares}},
  \code{qmao:::getHoldings.iShares.AsOf},
  \code{\link{getHoldings.vaneck}},
  \code{\link{getHoldings.powershares}},
  \code{\link{getHoldings.GlobalX}},
  \code{\link{getHoldings.FirstTrust}},
  \code{\link{getHoldings.WisdomTree}}
}

