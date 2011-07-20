#' Get Spread Dividends
#'
#' Given a spread_id, this will look for dividend data for each of the
#' constituent legs of the spread in the the env specified.  If dividend
#' data are not in \code{env} a call will be made to \code{getSymbols}
#' The memberratio (stored in the spread instrument) will be used to weight
#' the dividends.  Negative values in \sQuote{memberratio} indicate short
#' positions.  Dividends on short legs will be negative.
#' @param spread_id chr primary identifier for an already defined spread instrument 
#' @param from Date from which to get dividends
#' @param to get dividends through this date
#' @param auto.assign if TRUE (default), spread dividends will be stored in an environment
#' @param env where to get data. If \code{auto.assign} is TRUE it is also where the spread dividends will be stored
#' @return if \code{auto.assign} is TRUE the output will be written in the env specified with a .div appended to the name, and only the name of the symbol where the dividends were stored will be returned.
#' 
#' otherwise, an xts object containing dividends with negative values indicating payments in leiu of dividends.
#' @examples
#' \dontrun{
#' s <- define_stocks(c("SPY","DIA"))
#' spread('spydia', 'USD', members=s, memberratio=c(1,-1)) 
#' getSpreadDividends('spydia')
#' spydia.div
#' }
#' @export
getSpreadDividends <- function(spread_id, from='2007-01-01', to=Sys.Date(), auto.assign=TRUE, env=.GlobalEnv) {
	sp_instr <- try(getInstrument(spread_id))
	if (inherits(sp_instr, "try-error") || !is.instrument(sp_instr)) {
    	stop(paste("Instrument", spread_id , " not found; please create it."))
	}
	if (!inherits(sp_instr, "spread")) 
    	stop(paste("Instrument", spread_id , " is not a spread, please use the symbol of a spread instrument."))

    divs <- xts()
    for (i in 1:length(sp_instr$members)) {
        instr <- try(getInstrument(sp_instr$members[i]),silent=TRUE)
        if (inherits(instr, 'try-error') || !is.instrument(instr))
            stop(paste("Member instrument", sp_instr$members[i], " not found, please create it."))    

        #leg <- try(get(sp_instr$members[i], pos=env), silent=TRUE)
        #if (inherits(leg,'try-error')) {
		#    leg <- getSymbols(sp_instr$members[i], auto.assign=FALSE)		
	    #}
	    #from = max(as.Date(from), start(leg))
        #to = min(as.Date(to), end(leg))
       
	    div <- try(get(paste(sp_instr$members[1],"div",sep="."),pos=env),silent=TRUE)
	    if (inherits(div,'try-error') || is.null(div)) {
		    div <- getDividends(sp_instr$members[i], from=from, to=to, auto.assign=FALSE)
	    }
        div <- div * sp_instr$memberratio[i] #weight it
	    divs <- cbind(divs,div)
    }
	divs <- divs[paste(from,to,sep="::")]
    divs <- as.xts(apply(divs, 2, FUN=function(x) {x[is.na(x)] <- 0; x}))

    ret <- xts(rowSums(divs), order.by=index(divs))
    colnames(ret) <- paste(spread_id,'div',sep='.')
	if (auto.assign) {
		assign(paste(spread_id,'div',sep='.'), ret, pos=env)
		paste(spread_id,"div",sep='.')
	} else ret
}

