\name{BAM}
\alias{BAM}
\alias{BATM}
\title{Extract Bid, Ask, Mid columns or Bid, Ask, Mid, Trade columns}
\usage{
  BAM(x)

  BATM(x)
}
\arguments{
  \item{x}{xts object that includes at least the columns
  that are to be extracted.}
}
\value{
  xts object
}
\description{
  Similar to OHLC and BBO
}
\author{
  Garrett See, but based on -- and mostly copied -- from
  Jeff Ryan's quantmod
}
\seealso{
  OHLC, BBO is.BAM, is.BATM
}

