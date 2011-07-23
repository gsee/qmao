#'

PF <- makePriceFrame <-
function(symbols, from=NULL, to=NULL, prefer=NULL, notional=TRUE, na.omit=TRUE, env=.GlobalEnv, silent=FALSE) {
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
        pframe <- cbind(pframe, estAd(get(symbols[i],pos=env),prefer=prefer) * mult[i], all=TRUE)
    }
    if (na.omit) pframe <- na.omit(pframe)
    if (is.null(from)) from <- first(index(pframe))
    if (is.null(to)) to <- last(index(pframe))
    pframe[paste(from,to,sep="::")]
}

RF <- makeReturnFrame <- function(symbols, from=NULL, to=NULL, prefer=NULL, notional=TRUE, na.omit=TRUE, env=.GlobalEnv, silent=FALSE, ...) {
    frame <- makePriceFrame(symbols,from,to,prefer,notional,na.omit,env,silent)
    ROC(frame, ...)
}






