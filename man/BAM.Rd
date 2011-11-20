\name{BAM}
\alias{BAM}
\alias{BATM}
\title{Extract Bid, Ask, Mid columns or Bid, Ask, Mid, Trade columns}
\usage{
  BAM(x, symbol.name = NULL)

  BATM(x, symbol.name = NULL)
}
\arguments{
  \item{x}{xts object that includes at least the columns
  that are to be extracted.}

  \item{symbol.name}{used only if the Mid column has to be
  calculated for creating the column name.}
}
\value{
  xts object
}
\description{
  Similar to OHLC, but if the \sQuote{Mid} column does not
  exist, it will be calculated.
}
\author{
  Garrett See, but based on -- and mostly copied from --
  Jeff Ryan's quantmod
}
\seealso{
  OHLC, BBO is.BAM, is.BATM
}

