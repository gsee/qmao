\name{getSymbols.pos}
\alias{getSymbols.pos}
\title{getSymbols from an environment}
\usage{
  getSymbols.pos(Symbols, pos = .GlobalEnv, verbose = TRUE,
    ...)
}
\arguments{
  \item{Symbols}{character vector of names of objects}

  \item{pos}{where to look for the object (see
  \code{\link{get}}); if omitted, the function will search
  as if the name of the object appeared unquoted in an
  expression.}

  \item{verbose}{be verbose?}

  \item{...}{other arguments}
}
\value{
  either the names of objects that were stored if called
  with \code{auto.assign=TRUE}, or an object of type
  \code{return.class}
}
\description{
  method to getSymbols from an environment
}
\examples{
\dontrun{
e <- new.env()
getSymbols("SPY", src='yahoo', env=e) #data stored at e$SPY
getSymbols("SPY", src='pos', pos=e) #fetch data from e and store in .GlobalEnv
}
}
\author{
  Garrett See
}

