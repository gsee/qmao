getPriceFrame <-
function(symbols) {
	if (length(symbols) < 2) stop("You must provide a list containing 2 instrument names")
	if (length(symbols) > 2) { 
		warning("only using 1st 2 symbols") 
		symbols <- symbols[1:2]
	}
	mult <- NULL
	for (Symbol in symbols) {
		tmp_instr <- try(getInstrument(Symbol))
		if (inherits(tmp_instr, "try-error") | !is.instrument(tmp_instr)) {
			warning(paste("Instrument", Symbol, " not found, using contract multiplier of 1"))
			mult <- c(mult,1)
		} else {
			mult <- c(mult,tmp_instr$multiplier)
		}		
	}
	#mult <<- mult
    #TODO: try get from .GlobalEnv. If try-error, getSymbols()	
    prc1 <- estAd(get(symbols[1],pos='.GlobalEnv')) * mult[1]
	prc2 <- estAd(get(symbols[2],pos='.GlobalEnv')) * mult[2]
	ab.prc <- merge(prc1,prc2,all=FALSE)
	ab.prc
}

