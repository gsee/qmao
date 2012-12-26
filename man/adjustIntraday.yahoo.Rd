\name{adjustIntraday.yahoo}
\alias{adjustIntraday.yahoo}
\title{Adjust intraday data for dividends and splits}
\usage{
  adjustIntraday.yahoo(Symbols, adjustVolume = TRUE,
    env = .GlobalEnv, auto.assign = FALSE, store.to = env,
    verbose = TRUE)
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
  is returned.  The Volume column (if there is one) will be
  moved to become the last column
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
  put there. Data should be in an xts object.  If there is
  a Volume column, it will be divided by the adjustmet
  ratio; all other columns will be multiplied by it.
}
\note{
  Currently, adjustBAM will adjust OHLC or BAM data using
  addition/subtraction of cash flows.  Also, it requires
  that there be a pre-calculated column with Adjusted
  prices.  On the other hand, adjustIntraday.yahoo will
  multiply the data by an adjustment ratio. Currently, the
  adjustment ratio does not account for splits.
  adjustIntraday.yahoo does not require a pre-calculated
  Adjusted column. I plan to make these functions more
  similar, and to make the names of the functions more
  meaningful/representative of what they actually do.
}
\author{
  gsee
}
\seealso{
  \code{qmao:::adjustBAM}
}

