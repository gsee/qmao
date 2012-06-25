# getSymbols from an environment
getSymbols.pos <- function (Symbols, pos=.GlobalEnv, verbose=TRUE, ...) 
{

    this.env <- environment()
    for(var in names(list(...))) {
        assign(var,list(...)[[var]], this.env)
    }

    # Find out if user provided a value for each formal
    if (hasArg.pos <- hasArg(pos)) .pos <- pos
    
    importDefaults("getSymbols.pos") 
    
    # Now get the values for each formal that we'll use if not provided
    # by the user and not found in the SymbolLookup table
    default.pos <- pos

    # quantmod:::getSymbols will provide auto.assign and env
    # so the next 2 if statements should always be TRUE
    auto.assign <- if(hasArg(auto.assign)) {auto.assign} else TRUE
    env <- if(hasArg(env)) {env} else .GlobalEnv 

    # make an argument matching function to sort out which values to use for each arg
    pickArg <- function(x, Symbol) {
        if(get(paste('hasArg', x, sep="."))) {
            get(paste(".", x, sep=""))
        } else if(!is.null(SymbolLookup[[Symbol]][[x]])) {
            SymbolLookup[[Symbol]][[x]]
        } else get(paste("default", x, sep="."))
    }

    SymbolLookup <- getSymbolLookup()
    fr <- NULL
    datl <- lapply(1:length(Symbols), function(i) {
        pos <- pickArg("pos", Symbols[[i]])
        if(isTRUE(verbose)) cat("loading ",Symbols[[i]],".....")
        fr <- get(Symbols[[i]], pos=pos)
        #fr <- quantmod:::convert.time.series(fr=fr,return.class=return.class)
        Symbols[[i]] <-make.names(Symbols[[i]]) 
        tmp <- list()
        tmp[[Symbols[[i]]]] <- fr
        if(isTRUE(verbose)) cat("done.\n")
        tmp     
    })


    if (length(Filter("+", lapply(datl, length))) == 0) {
        warning("No data found.")
        return(NULL) 
    }

    datl.names <- do.call(c, lapply(datl, names))
    missing <- Symbols[!Symbols %in% datl.names]
    if (length(missing) > 0) warning('No data found for ', paste(missing, collapse=" "))
    if(auto.assign) {
        #invisible(lapply(datl, function(x) if (length(x) > 0) assign(names(x), x[[1]], pos=env)))
        out <- Filter(function(x) length(x) > 0, datl)
        invisible(lapply(out, function(x) assign(names(x), x[[1]], pos=env)))
        return(datl.names)
    } else {
        #NOTE: Currently, NULLs aren't filtered out.  If there are data for any Symbol,
        # the returned list will have an element for each symbol requested even if some don't contain data.
        out <- lapply(datl, function(x) {
            if (length(x) > 0) x[[1]]
        })
        if (length(out) == 1)
            return(out[[1]])
        else {
            names(out) <- Symbols
            return(out)
        }
    }
}

