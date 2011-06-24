makePriceFrame <-
function(symbols, from=NULL, to=NULL, prefer=NULL) {
	mult <- NULL
	for (Symbol in symbols) {
		tmp_instr <- try(getInstrument(Symbol,silent=TRUE))
		if (inherits(tmp_instr, "try-error") || !is.instrument(tmp_instr)) {
			warning(paste("Instrument", Symbol, " not found, using contract multiplier of 1"))
			mult <- c(mult,1)
		} else {
			mult <- c(mult,tmp_instr$multiplier)
		}		
	}
    pframe <- NULL
    for (i in 1:length(symbols)) {
        pframe <- cbind(pframe, estAd(get(symbols[i],pos=.GlobalEnv)) * mult[i], all=FALSE)
    }
	na.omit(pframe)
    if (is.null(from)) from <- first(index(pframe))
    if (is.null(to)) to <- last(index(pframe))
    pframe[paste(from,to,sep="::")]
}

makeReturnFrame <- function(symbols, env=.GlobalEnv, from=NULL, to=NULL, ...) {
    frame <- makePriceFrame(symbols,from,to)
    ROC(frame, ...)
}



