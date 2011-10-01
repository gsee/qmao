#' Plot several instruments on the same chart
#' 
#' Plot the prices or cumulative return of prices of several instruments on the
#' same chart.
#' 
#' VERY simple wrappers for ts.plot.
#' 
#' prefer is passed to \code{getPrice}
#' 
#' Does not account for dividends/splits.
#' 
#' @aliases plotRelPerf plotInstruments plotBAT
#' @param symbols character strings. vector of names of xts objects containing
#' Close column
#' @param timespan only intended for intraday data. Should be something like
#' "T08:30/T15:00"
#' @param prefer which price column to use
#' @param env the environment that holds the xts data
#' @return plots a chart
#' @author Garrett See
#' @seealso ts.plot, chart_Series, ggplot2, cumsum, ROC, getPrice, environment
#' @examples
#' 
#' \dontrun{
#'     syms <- c('SPY','TLT','DBA')
#'     getSymbols(syms)
#'     plotRelPerf(syms)
#'     plotInstruments(syms)
#' 
#'     #Use intraday data from IB.
#'     #and demonstrate how you can put data in different environments    
#'     twsData <- new.env()     
#'     #The next block will take at least 60 seconds to run
#'     getBAT(twsSTK('TLT','SMART'),env=twsData)
#'     getBAT(twsSTK('DBA','SMART'),env=twsData)
#'     plotRelPerf(c('TLT','DBA'),env=twsData)    
#'     plotBAT(c('TLT','DBA'),env=twsData)
#' }
#' @export
#' @rdname plotRelPerf
plotRelPerf <- function(symbols, timespan="", prefer=NULL, env=.GlobalEnv) {
    if (is.xts(symbols)) stop('symbols must be a character vector')
    x <- xts()
    if (length(symbols) <= 1) stop('vector of symbol names required')    
    for (symbol in symbols) { 
        tmp <- try(getPrice(na.omit(get(symbol,pos=env)),prefer=prefer)[,1],TRUE)
        if (!inherits(tmp, 'try-error') && length(tmp))
            x <- cbind(x,cumsum(ROC(tmp,na.pad=FALSE)))
    }
    ts.plot(x[timespan],col=rainbow(NCOL(x)))
}

#symbols <- c('SPY','DIA','QQQ')
#getSymbols(symbols)
#plotRelPerf(symbols)

#' @export
#' @rdname plotRelPerf
plotInstruments <- function(symbols,timespan="",prefer=NULL, env=.GlobalEnv) {
    if (is.xts(symbols)) stop('symbols must be a character vector')
    x <- xts()
    if (length(symbols) <= 1) stop('vector of symbol names required')    
    for (symbol in symbols) { 
        tmp <- try(getPrice(na.omit(get(symbol,pos=env)),prefer=prefer)[,1],TRUE)
        if (!inherits(tmp, 'try-error') && length(tmp))
           x <- cbind(x, tmp)
    }
    ts.plot(x[timespan],col=rainbow(NCOL(x)))
}

#plotInstruments(c("VIX_JAN11","VIX_FEB11","VIX_MAR11","VIX_APR11"))

#' @export
#' @rdname plotRelPerf
plotBAT <- function(symbols, timespan="", prefer=NULL, env=.GlobalEnv) {
    if (is.xts(symbols)) stop('symbols must be a character vector')
    x <- xts()
    for (symbol in symbols) {
        b <- try(getPrice(na.omit(get(symbol,pos=env)),prefer="bid")[,1],TRUE)
        a <- try(getPrice(na.omit(get(symbol,pos=env)),prefer="ask")[,1],TRUE)     
        tr <- try(getPrice(na.omit(get(symbol,pos=env)),prefer=prefer)[,1],TRUE)
        if (!any(sapply(list(b,a,tr), inherits, 'try-error'))) 
        x <- cbind(x, b, a, tr)
    }
    ts.plot(x[timespan],col=c("blue","red","black"))
}

