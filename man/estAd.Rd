\name{estAd}
\alias{estAd}
\alias{Mi}
\alias{Bi}
\alias{As}
\alias{Ch}
\alias{Tr}
\title{
get column containing Price
}
\description{
getPrice returns values in Price column
estAd returns values in Adjusted column if it exists, or it's best guess if it doesn't
}
\usage{
estAd(x, symbol=NULL, prefer=NULL)
Mi(x)
Bi(x)
As(x)
Ch(x)
Tr(x)
}

\arguments{
  \item{x}{
xts object
}
  \item{symbol}{
character string
}
  \item{prefer}{
for \code{estAd} what column to get if none of the defaults can be found.
}
}
\details{
If \code{prefer} is not NULL, \code{estAd} will simply call \code{\link{getPrice}} with \code{symbol} 
and \code{prefer}.  Otherwise, \code{estAd} will look for "adjusted", "close", "mid", "price", "trade", 
in that order.  If and only if those columns cannot be found, \code{symbol} will be passed in a call to 
\code{getPrice}

Bi, As, Ch, Tr display Bid, Ask, Change, Trade data.
}
\value{
xts object with only one column of prices
}
\author{
Garrett See
}
\note{
case insensitive
}
\seealso{
getPrice, Op, Hi, Lo, Cl
}
\examples{

getSymbols('SPY')
estAd(SPY)
}
