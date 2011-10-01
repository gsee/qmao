\name{estAd}
\alias{estAd}
\title{get column containing Price}
\usage{
  estAd(x, symbol = NULL, prefer = NULL)
}
\arguments{
  \item{x}{xts object}

  \item{symbol}{character string}

  \item{prefer}{for \code{estAd} what column to get if none
  of the defaults can be found.}
}
\value{
  xts object with only one column of prices
}
\description{
  \code{estAd} returns values in Adjusted column if it
  exists, or it's best guess if it doesn't
}
\details{
  If \code{prefer} is not NULL, \code{estAd} will simply
  call \code{\link{getPrice}} with \code{symbol} and
  \code{prefer}.  Otherwise, \code{estAd} will look for
  "adjusted", "close", "mid", "price", "trade", in that
  order.  If and only if those columns cannot be found,
  \code{symbol} will be passed in a call to \code{getPrice}
}
\note{
  case insensitive
}
\examples{
\dontrun{
getSymbols('SPY')
estAd(SPY)
}
}
\author{
  Garrett See
}
\seealso{
  \code{\link{getPrice}}, Op, Hi, Lo, \code{\link{Cl}}, Bi,
  As, \code{\link{Mi}}, Tr
}

