alignSymbols <-
function(symbols, env=.GlobalEnv) {
	if (length(symbols) < 2) 
		stop("Must provide at least 2 symbols")
	if (any(!is.character(symbols))) 
		stop("Symbols must be vector of character strings.")
	ff <- try(get(symbols[1],env=env))
	if (inherits(ff, 'try-error')) stop('You must create an xts object for each symbol first.')
	ncols <- ncol(ff)	#compare all symbols ncols to 1st one; make sure they're the same
	for (sym in symbols[-1]) {
		tmp.sym <- get(sym,pos=env)
		if (ncol(tmp.sym) != ncols) stop("All symbols must have the same number of columns.")
		ff <- merge(ff,tmp.sym,all=FALSE)
		ff <- na.omit(ff)
	}
	for (sym in symbols) {
		assign(sym,ff[,grep(sym,colnames(ff))],pos=env)
	}
	symbols
}

