
plotRelPerf <- function(symbols, timespan="", prefer=NULL, env=.GlobalEnv) {
    x <- xts()
    if (length(symbols) <= 1) stop('vector of symbol names required')    
    for (symbol in symbols) { 
        x <- cbind(x,cumsum(ROC(getPrice(na.omit(get(symbol,pos=env)),prefer=prefer)[,1],na.pad=FALSE)))
    }
    ts.plot(x[timespan],col=rainbow(NCOL(x)))
}

#symbols <- c('SPY','DIA','QQQ')
#getSymbols(symbols)
#plotRelPerf(symbols)

plotInstruments <- function(symbols,timespan="",prefer=NULL, env=.GlobalEnv) {
    x <- xts()
    if (length(symbols) <= 1) stop('vector of symbol names required')    
    for (symbol in symbols) { 
        x <- cbind(x,getPrice(na.omit(get(symbol,pos=env)),prefer=prefer)[,1])
    }
    ts.plot(x[timespan],col=rainbow(NCOL(x)))
}

#plotInstruments(c("VIX_JAN11","VIX_FEB11","VIX_MAR11","VIX_APR11"))

plotBAT <- function(symbols, timespan="", prefer=NULL, env=.GlobalEnv) {
    x <- xts()
    for (symbol in symbols) {
        x <- cbind(x, getPrice(na.omit(get(symbol,pos=env)),prefer="bid")[,1],
                    getPrice(na.omit(get(symbol,pos=env)),prefer="ask")[,1],
                    getPrice(na.omit(get(symbol,pos=env)),prefer=prefer)[,1])
    }
    ts.plot(x[timespan],col=c("blue","red","black"))
}


