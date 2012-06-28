\name{read.masterDATA}
\alias{read.masterDATA}
\title{Read masterDATA csv}
\usage{
  read.masterDATA()
}
\value{
  a \code{data.frame} with a column for \sQuote{Name} and
  \code{Symbol}
}
\description{
  read the Name and ticker Symbol of most ETFs from
  \href{http://www.masterdata.com/helpfiles/ETF_List_Downloads/AllTypes.csv}{this
  csv} at
  \href{http://www.masterdata.com/HelpFiles/ETF_List.htm}{masterdata.com}.
}
\details{
  This is used by \code{\link{getHoldings}} to determine
  how to find the holdings of an ETF.  It is also used by
  \code{\link{getHoldings.vaneck}} to create a list of Van
  Eck (Market Vectors) Symbols to use.
}
\examples{
\dontrun{
(etfs <- read.masterDATA())
}
}
\author{
  Garrett See
}

