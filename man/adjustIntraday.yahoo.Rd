\name{adjustIntraday.yahoo}
\alias{adjustIntraday.yahoo}
\title{Adjust intraday data for dividends and splits}
\usage{
  adjustIntraday.yahoo(Symbols, adjustVolume = TRUE, env =
  .GlobalEnv, auto.assign = FALSE, store.to = env, verbose
  = TRUE)
}
\arguments{
  \item{Symbols}{character name of xts object}

  \item{adjustVolume}{if \code{TRUE} (default) Volume will
  be divided by the adjustment ratio}

  \item{env}{the environment where \code{x} is stored}

  \item{auto.assign}{assign the adjusted data in the
  environment named \code{store.to}?}

  \item{store.to}{environment in which to store the
  adjusted data (if \code{auto.assign=TRUE})}

  \item{verbose}{cat progress info to screen?}
}
\value{
  if \code{auto.assign=TRUE} (default) the name of the xts
  object is returned.  Otherwise, the adjusted xts object
  is returned.
}
\description{
  Alpha code! Subject to change in the near future! A daily
  adjustment ratio is calculated using dividend data that
  is downloaded from yahoo.  The ratio is then applied to
  the data on an intraday basis.
}
\details{
  If the instrument object does not have dividend data in a
  slot named \dQuote{div}, the data will be downloaded and
  put there. Data should be in an xts object with columns:
  \sQuote{Bid}, \sQuote{Ask}, \sQuote{Trade}, \sQuote{Mid},
  \sQuote{Volume}
}
\note{
  Currently, adjustBAM will adjust OHLC or BAM data using
  addition/subtraction of cash flows.  Also, it requires
  that there be a pre-calculated column with Adjusted
  prices.  On the otherhand, adjustIntraday.yahoo will only
  adjust BATM data, and it will adjust it by multiplying
  the data by an adjustment ratio.  The adjustment ratio
  does not account for splits.  adjustIntraday.yahoo does
  not require a pre-calculated Adjusted column. I plan to
  make these functions more similar, and to make the names
  of the functions more meaningful/representative of what
  they actually do.
}
\author{
  gsee
}
\seealso{
  \code{\link{adjustBAM}}
}

