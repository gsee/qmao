\name{ClOp}
\alias{ClOp}
\title{
Close to Open return
}
\description{
Calculates returns from previous days' closes to current days' opens
}
\usage{
ClOp(x)
}

\arguments{
  \item{x}{
an xts object with Close and Open columns
}
}
\details{

}
\value{
an xts object of returns
}
\references{

}
\author{
Garrett See
}
\note{

}


\seealso{
OpOp, ClCl, OpCl
}
\examples{
getSymbols("SPY")
ClOp(SPY)
}
