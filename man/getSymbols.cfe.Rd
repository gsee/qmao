\name{getSymbols.cfe}
\alias{getSymbols.cfe}
\title{Load Data from the CBOE Futures Exchange website...}
\usage{
  getSymbols.cfe(Symbols, Months = NULL, Years = NULL,
    from = "2004-06-01", to = Sys.Date(),
    nonzero.close = TRUE, rescale = TRUE, env,
    return.class = "xts", index.class = "Date", ...)
}
\arguments{
  \item{Symbols}{vector of names of instruments formated
  like e.g. "VX_U09", "GV_Z10", etc. OR if \code{Months}
  and \code{Years} are provided a vector of names of root
  symbols, eg. "VX","GV",etc.}

  \item{Months}{optional vector of months. Can be numerical
  or the names of the months in English. If any element of
  \code{Symbols} contains an underscore, this will be
  ignored.}

  \item{Years}{optional vector of years. Can either be 2 or
  4 digits each. If any element of \code{Symbols} contains
  an underscore, this will be ignored.}

  \item{from}{retrieve data no earlier than this data
  (2004-06-01)}

  \item{to}{retrieve data through this data (Sys.Date())}

  \item{nonzero.close}{if \code{TRUE} rows where
  \sQuote{Close} is zero will be removed.}

  \item{rescale}{Should data from before March 26, 2007 be
  adjusted? See Details and References. Only applicable if
  \code{Symbols} is "VX" or "DV".}

  \item{env}{where to create objects (.GlobalEnv)}

  \item{return.class}{class of returned object}

  \item{index.class}{class of returned object index (xts
  only)}

  \item{\dots}{additional arguments}
}
\value{
  will load data into the specified environment -- one
  object for each file downloaded.
}
\description{
  Load Data from the CBOE Futures Exchange website
}
\details{
  Download \code{Symbols} to specified \code{env} from
  \sQuote{cfe.cboe.com}. This method is not to be called
  directly; instead, a call to \code{getSymbols(symbols,
  src='cfe')} will in turn call this method. This is a
  wrapper to get end-of-day historical data for the futures
  that are, or have been, listed on the CBOE Futures
  Exchange.

  \code{Symbols} can be a vector of symbols formatted like
  VX_U11, or GV_M10. Alternatively, the symbol roots can be
  used (e.g. 'VX', 'GV') and values can be provided for
  \code{Months} and \code{Years}. If roots are provided,
  but \code{Years} and \code{Months} is \code{NULL}, the
  current year as reported by \code{Sys.Date()} will be
  used, and either the current month, or, for "VT" and "VA"
  (which are quarterly contracts), the month of the end of
  the last quarter, or, for "RPXC", (which is biannual),
  the most recent March or Sep contract. See examples.

  The raw data will contain zero values for the first few
  rows. Also, the last row will have zeros in every column
  except the \sQuote{Settle} column. By default, these
  rows, along with any other rows that have a zero Close
  price will be removed.  If you would like to keep these
  rows (e.g. if you need the final settlement value) use
  \code{nonzero.close=FALSE}.

  On March 26, 2007, The prices of VX and DV futures
  contracts were rescaled (divided by 10) \dQuote{to bring
  the traded futures contract prices in line with the
  underlying index values...} If \code{rescale=TRUE}
  (Default) prices for these instruments from prior to
  March 26, 2007 will be divided by 10 ("rescaled").  See
  References.
}
\note{
  Currently listed contracts: \itemize{ \item{VSW -}{ CBOE
  Short Term Volatility Index Futures (VSW)} \item{VX -}{
  CBOE S&P 500 Volatility Index (VIX) Futures} \item{VU -}{
  CBOE Russell 2000 Volatility Index (RVX) Futures}
  \item{VN -}{ CBOE Nasdaq-100 Volatility Index (VXN)
  Futures (was delisted in 2009 and relisted in July 2012)}
  \item{VA -}{ S&P 500 Variance Futures (product launched
  December 10th, 2012)} \item{VXEM -}{ CBOE Emerging
  Markets ETF Volatility Index (VXEEM) Security Futures}
  \item{VXEW -}{ CBOE Brazil ETF Volatility Index (VXEWZ)
  Security Futures} \item{GV -}{ CBOE Gold ETF Volatility
  Index (GVZ) Security Futures} \item{OV -}{ CBOE Crude Oil
  ETF Volatility Index (OVX) Security Futures} }

  Delisted contracts: \itemize{ \item{VM -}{ CBOE Mini-VIX
  Futures} \item{RPXC -}{ Radar Logic 28-Day Real Estate
  Index (RPX) Future} \item{VT -}{ CBOE S&P 500 3-Month
  Variance Futures} \item{BX -}{ CBOE S&P 500 BuyWrite
  Index Futures (BX)} \item{VR -}{ RUSSELL 2000 Volatility
  Index Futures (VR)} \item{VA -}{ CBOE S&P 500 12-Month
  Variance Futures (VA) (product delisted March 18, 2011)}
  \item{DV -}{ CBOE DJIA Volatility Index (DV)} }
}
\examples{
\dontrun{
getSymbols(c("VX_U11", "VX_V11"),src='cfe')
#all contracts expiring in 2010 and 2011.
getSymbols("VX",Months=1:12,Years=2010:2011,src='cfe')
#getSymbols("VX",Months=1:12,Years=10:11,src='cfe') #same
#The contracts expiring this month:
getSymbols(c("VM","GV"),src='cfe')

setSymbolLookup(VX='cfe') #so we don't have to specify src anymore
getSymbols("VX",Months=1:3,Years=2005)
}
}
\author{
  Garrett See, based on Jeff Ryan's quantmod framework
}
\references{
  \url{http://cfe.cboe.com/Data/HistoricalData.aspx#VT},

  \url{http://tinyurl.com/CFE-VIX-VXN-Rescaling}
}
\seealso{
  \code{\link{remove_zero_rows}} for removing rows where a
  column has zero values. \code{getSymbols},
  \code{setSymbolLookup}
}

