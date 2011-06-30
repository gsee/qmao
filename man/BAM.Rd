\name{BAM}
\alias{BAM}
\alias{BATM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract Bid, Ask, Mid columns or Bid, Ask, Mid, Trade columns
}
\description{
Similar to OHLC and BBO
}
\usage{
BAM(x)
BATM(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
xts object that includes at least the columns that are to be extracted.
}
}
\value{
xts object
}
\author{
Garrett See, but based on -- and mostly copied -- from Jeff Ryan's quantmod
}
\seealso{
OHLC, BBO
is.BAM, is.BATM
}

