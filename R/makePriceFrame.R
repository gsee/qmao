#' Merge Adjusted prices (or returns of adjusted prices) of several symbols
#' 
#' Given the names of symbols, merge together Adjusted columns of their xts
#' objects
#' 
#' \code{PF} and \code{RF} are aliases. \code{makePriceFrame} merges price
#' columns of several symbols where the column is adjusted if it exists, or
#' close, mid, or price if it does not.  \code{makeReturnFrame} first calls
#' \code{makePriceFrame}, then calculates returns on those price series
#' 
#' The \code{from} and \code{to} arguments are intended to be used to indicate
#' the starting and ending date, whereas the \code{subset} argument is intended
#' to be used to subset intraday data by time of day.
#' 
#' @aliases makePriceFrame makeReturnFrame PF RF
#' @param symbols vector of character strings
#' @param from include no data before this date/timestamp
#' @param to include data through this date/timestamp
#' @param prefer column to use. If NULL, the first of the following columns
#' that is found will be used: \sQuote{Adjusted}, \sQuote{Close}, \sQuote{Mid}
#' @param notional Should the prices will be multiplied by their multipliers,
#' to get notional values? Default is \code{TRUE}
#' @param na.omit Should \code{NA} values be removed? Default is \code{TRUE}
#' @param subset xts style subsetting argument. (e.g. "T08:30/T15:00") Default
#' is \code{NULL}
#' @param env environment in which to look for xts objects
#' @param silent silence warnings? (Currently, the only warning message is
#' \dQuote{Instrument not found, using contract multiplier of 1})
#' @param \dots arguments to be passed to \code{ROC}.  These can be \sQuote{n},
#' \sQuote{type}, \sQuote{na.pad},
#' @return xts object with same number of columns as length of \code{symbols}:
#' 1 for each symbol's [adjusted] prices (returns)
#' @note \code{makeReturnFrame} can be useful before calling
#' charts.PerformanceSummary from the PortfolioAnalytics package.
#' @author Garrett See
#' @seealso estAd, getPrice, merge, cbind
#' @examples
#' 
#' \dontrun{
#' getSymbols(c('SPY','DIA','QQQ'))
#' pf <- makePriceFrame(c('SPY','DIA','QQQ'))
#' head(pf)
#' rf <- makeReturnFrame(c('SPY','DIA','QQQ'))
#' head(rf)
#' }
#' @export
PF <- makePriceFrame <-
function(symbols, from=NULL, to=NULL, prefer=NULL, notional=TRUE, na.omit=TRUE, subset=NULL, env=.GlobalEnv, silent=FALSE) {
	mult <- NULL
	for (Symbol in symbols) {
		tmp_instr <- try(getInstrument(Symbol,silent=TRUE))
		if (inherits(tmp_instr, "try-error") || !is.instrument(tmp_instr)) {
			if (!silent && notional) warning(paste("Instrument", Symbol, " not found, using contract multiplier of 1"))
			mult <- c(mult,1)
		} else {
			mult <- c(mult,as.numeric(tmp_instr$multiplier))
		}		
	}
    
    if (!notional) mult <- rep(1,length(mult))

    pframe <- NULL
    for (i in 1:length(symbols)) {
        tmp.dat <- try(estAd(get(symbols[i],pos=env),prefer=prefer),TRUE)
        if (!is.null(subset)) tmp.dat <- tmp.dat[subset]
        if (!inherits(tmp.dat,'try-error') && length(tmp.dat))
            pframe <- cbind(pframe, tmp.dat * mult[i], all=TRUE)
    }
    if (na.omit) pframe <- na.omit(pframe)
    if (is.null(from)) from <- first(index(pframe))
    if (is.null(to)) to <- last(index(pframe))
    pframe[paste(from,to,sep="/")]
}

RF <- makeReturnFrame <- function(symbols, from=NULL, to=NULL, prefer=NULL, notional=TRUE, na.omit=TRUE, subset=NULL, env=.GlobalEnv, silent=FALSE, ...) {
    frame <- makePriceFrame(symbols,from,to,prefer,notional,na.omit,subset,env,silent)
    ROC(frame, ...)
}

