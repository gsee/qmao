\name{gsa}
\alias{gsa}
\title{Get, Subset, Assign}
\usage{
  gsa(Symbols, subset = "T08:30/T15:00", env = .GlobalEnv,
    store.to = env, invisible = FALSE)
}
\arguments{
  \item{Symbols}{names of xts objects}

  \item{subset}{xts style subetting string.
  ("T08:30/T15:00")}

  \item{env}{environment where data is stored (.GlobalEnv)}

  \item{store.to}{environment in which to store the
  subsetted data. By default it is the same as \code{env}}

  \item{invisible}{TRUE/FALSE. If true, nothing is
  returned, otherwise, the name of symbols that were subset
  will be returned}
}
\description{
  Get data from \code{env}ironment, subset the data, and
  re-assign it.
}
\details{
  Primarily intended to remove data from outside of regular
  trading hours.  However, can be used for any xts style
  subsetting.
}
\examples{
\dontrun{
getSymbols('SPY', from='2011-01-01', to='2011-06-01', src='yahoo')
tail(SPY)
gsa("SPY","::2011-04-15")
tail(SPY)
}
}
\author{
  Garrett See
}
\seealso{
  \code{\link{gaa}}, \code{\link{alignSymbols}},
  \code{\link{PF}}
}

