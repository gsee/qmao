\name{do.call.rbind}
\alias{do.call.rbind}
\title{Faster way to \code{do.call(rbind, lst)}}
\usage{
  do.call.rbind(lst)
}
\arguments{
  \item{lst}{a list to be rbound. (No guaranteed this
  funtion will offer an improvement unless \code{lst} is a
  lits of xts objects)}
}
\description{
  This is a functionalized version of Dominik's answer
  here:
  \url{http://stackoverflow.com/questions/7224938/can-i-rbind-be-parallelized-in-r}
  it does what do.call(rbind, lst) would do, but faster and
  with less memory usage
}
\examples{
x <- xts(rnorm(10000), Sys.time()-10000:1*60)
splx <- split(x, 'days')
dcrx <- do.call.rbind(splx)
identical(x, dcrx)
}
\references{
  \url{http://stackoverflow.com/questions/7224938/can-i-rbind-be-parallelized-in-r}
}

