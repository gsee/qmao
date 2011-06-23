makePriceFrame <-
function(symbols) {
	if (length(symbols) < 2) stop("You must provide a list containing 2 instrument names")
	#if (length(symbols) > 2) { 
	#	warning("only using 1st 2 symbols") 
	#	symbols <- symbols[1:2]
	#}
	mult <- NULL
	for (Symbol in symbols) {
		tmp_instr <- try(getInstrument(Symbol))
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
}

makeReturnFrame <- function(symbols, env=.GlobalEnv, ...) {
    frame <- makePriceFrame(symbols)
    ROC(frame, ...)
}
