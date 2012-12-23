\name{shinyBATS}
\alias{shinyBATS}
\title{shiny BATS quotes}
\usage{
  shinyBATS(port = 8100L, launch.browser = interactive())
}
\arguments{
  \item{port}{The TCP port that the application should
  listen on.  Defaults to port 8100.}

  \item{launch.browser}{If \code{TRUE}, the system's
  default web browser will be launched automatically after
  the app is started.  Defaults to \code{TRUE} in
  interactive sessions only.}

  \item{...}{arguments to \code{\link[shiny]{runApp}}}
}
\description{
  Start a shiny application that shows the top five levels
  of the BATS order book for a stock.
}
\details{
  This shiny app simply calls \code{\link{getQuote.BATS}}
  and plots the result. It requests new data every second,
  but the data from BATS only updates every five seconds.

  The code for this shiny app can be found in the shinyBATS
  directory at the top level of this package.
}
\note{
  documentation for \code{shiny} arguments copied from
  \code{?shiny::runApp}
}
\author{
  Garrett See
}
\seealso{
  \code{\link{getQuote.BATS}}, \code{\link{getQuote}},
  \code{\link[shiny]{runApp}}
}

