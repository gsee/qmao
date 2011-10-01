\name{ClOp}
\alias{ClOp}
\title{Close to Open return}
\usage{
  ClOp(x)
}
\arguments{
  \item{x}{an xts object with Close and Open columns}
}
\value{
  an xts object of returns
}
\description{
  Calculates returns from previous days' closes to current
  days' opens
}
\examples{
\dontrun{
getSymbols("SPY")
ClOp(SPY)
}
}
\author{
  Garrett See
}
\seealso{
  OpOp, ClCl, OpCl
}

