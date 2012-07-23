getHoldings.skeleton <- function(Symbols, env=.GlobalEnv, auto.assign=TRUE) {
    ## Either find "all possible Symbols" for this source, then if "Symbols" is
    ## missing, replace Symbols with "all possible Symbols", or if "Symbols" is 
    ## not missing, replace it with those Symbols that are also in 
    ## "all possible Symbols"
    ## e.g. getHoldings.SPDR looks like this:
    #
    # s <- SPDRSymbols() # this downloads all SPDR symbols
    # Symbols <- if (missing(Symbols)) { s } else Symbols[Symbols %in% s]
    # if (length(Symbols) == 0L) { return(NULL) }
    #
    ## OR
    ## if you do not want to check to make sure that all "Symbols" are actually
    ## Symbols that this source can provide holdings for, you can only 
    ## tinker with Symbols if it is missing like this
    if (missing(Symbols)) {
        ## Put code here to assign to Symbols a vector of all Symbols that are 
        ## available from this source.  Preferably, this comes from the source,
        ## but not all fund providers make it easy to get a list of their ETFs.
        ## As a last resort, you can use the csv from masterDATA
        #
        #etfs <- read.masterDATA()
        #etfs[, 1] <- sapply(strsplit(etfs[, 1], " "), "[", 1)
        #first.word.of.provider <- "WisdomTree" ## Just a random example
        #Symbols <- etfs[etfs[["Name"]] == first.word.of.provider, "Symbol"] 
        Symbols <- "XXX" # Replace this with a vector of ETF symbols for which 
                         # to get holdings
    }
    if (length(Symbols) == 0L) { return(NULL) }
    hlist <- lapply(Symbols, function(Symbol) {
        if (length(Symbols) > 1) {
            message(paste("Getting holdings for", Symbol))
        }
        ## Download holdings into data.frame.
        # e.g.
        #URL <- paste("http://some.file/to.download/", Symbol, sep="")
        #tmpfile <- tempfile()
        #download.file(URL, destfile=tmpfile, quiet=TRUE)
        #dat <- read.csv(tmpfile, stringsAsFactors=FALSE)
        #unlink(tmpfile)
        #if (inherits(dat, 'try-error')) { return(NULL) }
        #
        ## First column needs to be Symbol.Weight. e.g. for an ETF with
        ## ticker XXX, the first column name should be XXX.Weights
        ## The 1st column should contain the weights of the holdings between
        ## 0-100 and summing to 100.
        #colnames(dat)[1] <- paste(Symbol, "Weight", sep=".")
        ## Column 2 should be called "Name" and should contain a description of
        ## the holdingg
        #
        ## Find the column that contains the ticker and the column that contains
        ## the weights.  The put the columns in the order we want. This is only
        ## a guide and may/should be different depending on the data
        #tcol <- grep("ticker|symbol", colnames(out), ignore.case=TRUE)
        #if (length(tcol) == 1L) {
        #    #rownames should (ideally) be ticker symbols of holdings
        #    rownames(dat) <- make.names(dat[, tcol], unique=TRUE) 
        #    dat <- dat[, -tcol]
        #}
        #wcol <- grep("weight", colnames(dat), ignore.case=TRUE)
        #if (length(wcol) > 0L) { 
        #    colnames(dat)[wcol] <- paste(Symbol, "Weight", sep=".") 
        #}
        #out <- dat[, unique(c(wcol, seq_len(NCOL(dat))))]
        #
        ## check to see if any holdings' symbols are duplicated; if so, add a 
        ## duplicates attr
        #dupes <- character(0)
        #if (any(duplicated(dat[, tcol]))) {
        #    dupes <- dat[, tcol][duplicated(dat[, tcol])]
        #    warning(paste(Symbol, "has some holdings with duplicate Symbols:", 
        #                  paste(dupes, collapse=" ")))
        #} 
        #if (length(dupes) > 0) attr(dat, "duplicates") <- dupes
        #
        ## class as holdings, data.frame.
        #class(dat) <- c("holdings", "data.frame")
        #out
    })
    names(hlist) <- Symbols
    if (isTRUE(auto.assign)) {
        sout <- do.call(c, lapply(Symbols, function(x) {
            if (!is.null(hlist[[x]])) {
                assign(paste(x, "h", sep="."), hlist[[x]], pos=env)
                x
            }
        }))
        if (length(sout) > 0){
            return(paste(sout, "h", sep="."))
        } else return(NULL)
    }
    if (length(hlist) > 1) { 
        return(hlist)
    } else return(hlist[[1]])
}
