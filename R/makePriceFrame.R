#' Merge Adjusted prices (or returns of adjusted prices) of several symbols
#' 
#' Given the names of symbols, merge together Adjusted columns of their xts
#' objects
#' 
#' \code{PF} and \code{RF} are aliases. \code{makePriceFrame} merges the \code{prefer}
#' columns of several symbols if \code{prefer} is given.  Otherwise, it will use
#'  the adjusted column is if it exists, or close, mid, or price if it does not. 
#' (See \code{\link{estAd}}).
#'
#' If \code{notional} is \code{TRUE} and any \code{Symbols} are the names of 
#' \code{\link[FinancialInstrument]{instrument}}s, prices will be multiplied by
#' their multipliers.
#' 
#' \code{makeReturnFrame} first calls \code{makePriceFrame}, then calculates 
#' returns on those price series
#' 
#' The \code{from} and \code{to} arguments are intended to be used to indicate
#' the starting and ending date, whereas the \code{subset} argument is intended
#' to be used to subset intraday data by time of day.
#' 
#' Since this function merges together the same \code{prefer} column from each Symbol,
#' the column names can be simplified.  The \code{colnames} will simply be set
#' to be the same as \code{Symbols}.  A "prefer" \code{attr} will be added to the 
#' returned object so that you can see (using \code{str}, or \code{attr(x, "prefer")}) 
#' what column was used to build the PriceFrame.
#' @aliases makePriceFrame makeReturnFrame PF RF
#' @param Symbols character vector of names of xts objects
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
#' @return xts object with same number of columns as length of \code{Symbols}, 
#' 1 for each symbol's [adjusted] prices (returns). The object will have a
#' "prefer" \code{attr} indicating which column was used to build it.
#' @note \code{makeReturnFrame} can be useful before calling functions like
#' charts.PerformanceSummary from the PortfolioAnalytics package.
#' @author Garrett See
#' @seealso \code{\link{estAd}}, \code{\link[quantmod]{getPrice}}, 
#' \code{\link[base]{attr}}, \code{\link{gsa}}, \code{\link{alignSymbols}}, merge, cbind
#' @examples
#' 
#' \dontrun{
#' getSymbols(c('SPY','DIA','QQQ'))
#' pf <- makePriceFrame(c('SPY','DIA','QQQ'))
#' head(pf)
#' rf <- makeReturnFrame(c('SPY','DIA','QQQ'))
#' head(rf)
#' ## See what column was used to make pf
#' attr(pf, "prefer") 
#' }
#' @export
#' @rdname PF
makePriceFrame <-
function(Symbols, from=NULL, to=NULL, prefer=NULL, notional=TRUE, na.omit=TRUE, subset=NULL, env=.GlobalEnv, silent) {
    if (any(sapply(Symbols, exists, env) == FALSE))
       stop(paste("No data in", deparse(substitute(env)), "for", Symbols[!sapply(Symbols, exists, .GlobalEnv)]))
    if (missing(silent)) {
        silent <- ifelse(is.environment(get('.instrument', pos=.GlobalEnv)), FALSE, TRUE)
    }
	mult <- NULL
	for (Symbol in Symbols) {
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
    for (i in 1:length(Symbols)) {
        tmp.dat <- try(estAd(get(Symbols[i],pos=env),prefer=prefer),TRUE)
        if (!inherits(tmp.dat,'try-error') && length(tmp.dat)) {
            if (!is.null(subset)) tmp.dat <- tmp.dat[subset]
            pframe <- cbind(pframe, tmp.dat * mult[i], all=TRUE)
        }
    }
    if (na.omit) pframe <- na.omit(pframe)
    if (is.null(from)) from <- first(index(pframe))
    if (is.null(to)) to <- last(index(pframe))
    pframe <- pframe[paste(from,to,sep="/")]
    attr(pframe, "prefer") <- sub("\\.", "", gsub(Symbols[[1]], "", colnames(pframe)[[1]]))
    colnames(pframe) <- Symbols
    pframe
}

#' @export
#' @rdname PF
PF <- makePriceFrame

#' @export
#' @rdname PF
makeReturnFrame <- function(Symbols, ..., from=NULL, to=NULL, prefer=NULL, notional=TRUE, na.omit=TRUE, subset=NULL, env=.GlobalEnv, silent) {
    if (missing(silent)) {
        silent <- ifelse(is.environment(get('.instrument', pos=.GlobalEnv)), FALSE, TRUE)
    }
    frame <- makePriceFrame(Symbols,from,to,prefer,notional,na.omit,subset,env,silent)
    ROC(frame, ...)
}

#' @export
#' @rdname PF
RF <- makeReturnFrame
