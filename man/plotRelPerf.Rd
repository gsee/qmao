\name{plotRelPerf}
\alias{plotBAT}
\alias{plotInstruments}
\alias{plotRelPerf}
\title{Plot several instruments on the same chart}
\usage{
  plotRelPerf(symbols, timespan = "", prefer = NULL,
    env = .GlobalEnv)

  plotInstruments(symbols, timespan = "", prefer = NULL,
    env = .GlobalEnv)

  plotBAT(symbols, timespan = "", prefer = NULL,
    env = .GlobalEnv)
}
\arguments{
  \item{symbols}{character strings. vector of names of xts
  objects containing Close column}

  \item{timespan}{only intended for intraday data. Should
  be something like "T08:30/T15:00"}

  \item{prefer}{which price column to use}

  \item{env}{the environment that holds the xts data}
}
\value{
  plots a chart
}
\description{
  Plot the prices or cumulative return of prices of several
  instruments on the same chart.
}
\details{
  VERY simple wrappers for ts.plot.

  prefer is passed to \code{getPrice}

  Does not account for dividends/splits.
}
\examples{
\dontrun{
    syms <- c('SPY','TLT','DBA')
    getSymbols(syms)
    plotRelPerf(syms)
    plotInstruments(syms)

    #Use intraday data from IB.
    #and demonstrate how you can put data in different environments
    twsData <- new.env()
    #The next block will take at least 60 seconds to run
    getBAT(twsSTK('TLT','SMART'),env=twsData)
    getBAT(twsSTK('DBA','SMART'),env=twsData)
    plotRelPerf(c('TLT','DBA'),env=twsData)
    plotBAT(c('TLT','DBA'),env=twsData)
}
}
\author{
  Garrett See
}
\seealso{
  ts.plot, chart_Series, ggplot2, cumsum, ROC, getPrice,
  environment
}

