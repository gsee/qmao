#' Adjust intraday data for dividends and splits
#' 
#' Alpha code! Subject to change in the near future!
#' A daily adjustment ratio is calculated using
#' dividend data that is downloaded 
#' from yahoo.  The ratio is then applied to 
#' the data on an intraday basis.
#'
#' If the instrument object does not
#' have dividend data in a slot named 
#' \dQuote{div}, the data will be downloaded
#' and put there.
#' Data should be in an xts object.  If there is a Volume column, it will be
#' divided by the adjustmet ratio; all other columns will be multiplied by it.
#' @param Symbols character name of xts object
#' @param adjustVolume if \code{TRUE} (default) Volume will be divided by the adjustment ratio
#' @param env the environment where \code{x} is stored
#' @param auto.assign assign the adjusted data in the environment named \code{store.to}?
#' @param store.to environment in which to store the adjusted data (if \code{auto.assign=TRUE})
#' @param verbose cat progress info to screen?
#' @return if \code{auto.assign=TRUE} (default) the name of the xts object is 
#' returned.  Otherwise, the adjusted xts object is returned.  The Volume
#' column (if there is one) will be moved to become the last column
#' @seealso \code{\link{adjustBAM}}
#' @author gsee
#' @note Currently, adjustBAM will adjust OHLC or BAM data using 
#' addition/subtraction of cash flows.  Also, it requires that there be a 
#' pre-calculated column with Adjusted prices.  On the otherhand, 
#' adjustIntraday.yahoo will multiply the data by an adjustment ratio.  
#' Currently, the adjustment ratio does not account for splits.  
#' adjustIntraday.yahoo does not require a pre-calculated Adjusted column.
#' I plan to make these functions more similar, and to make the names of the 
#' functions more meaningful/representative of what they actually do.
#' @export
adjustIntraday.yahoo <- function(Symbols, adjustVolume=TRUE, env=.GlobalEnv, 
                                 auto.assign=FALSE, store.to=env, verbose=TRUE) {
    if (auto.assign) {
        if (!is.environment(store.to)) stop(store.to, " is not an environment") 
    } else if (length(Symbols) > 1) stop("auto.assign must be TRUE if length(Symbols) > 1.")
    symout <- NULL
    for (sym in Symbols) {
        symdata <- try(get(sym, pos=env), silent=TRUE)
        if (inherits(symdata, 'try-error')){
           if (verbose) cat("\nData not found in", deparse(substitute(env)), " ... skipping", sym, "!!!\n")
           next
        }
        if (isTRUE(attr(symdata, "adj"))) {
            # at the end of this function, I give the object an attr "adj" with a value of TRUE.
            # Here we check. If there is an 'adj' attr that is TRUE, then this data has already been adjusted
            if (verbose) cat(sym, 'data has already been adjusted ... skipping. \n')
            next 
        }
        div <- try(get_div(sym))
    #    spl <- get_spl(sym)
        if (is.null(div)) div <- NA
    #    if (is.null(spl)) spl <- NA
        cls <- if (!quantmod:::has.Mid(symdata) && !(has.Bid(symdata) && has.Ask(symdata))) {
            if (has.Cl(symdata)) {
                Cl(symdata)
            } else if (has.Trade(symdata)) {
                Tr(symdata)
            } else if (NCOL(symdata) == 1) {
                symdata
            } else stop("data should have a Mid, Close, Trade column, or only have 1 column.")
        } else Mi(symdata)
        cls <- to.daily(cls, OHLC=FALSE)
        index(cls) <- as.Date(index(cls))
        colnames(cls) <- gsub('Mid','Close',colnames(cls),ignore.case=TRUE)
        drat <- adjRatios(dividends=div, close=cls) #, splits=spl
        drat <- drat[, 1] * drat[, 2]
        idrat <- cbind(symdata[, 1], drat) 
        idrat[,2] <- na.locf(idrat[,2])
        idrat <- as.numeric(idrat[!is.na(idrat[,1])][,2]) #intraday ratio

        # muliply prices by ratio; divide Volume by ratio (if it has Volume)
        x <- if (has.Vo(symdata)) {
            cbind(coredata(symdata[, -has.Vo(symdata, which=TRUE)]) * idrat, 
                  coredata(Vo(symdata)) / idrat)
        } else coredata(symdata) * idrat
        xcoredata(x) <- xcoredata(symdata)
        attr(x, 'adj') <- TRUE
        if (auto.assign) {
            assign(sym, x, pos=store.to)
            symout <- c(symout, sym)
        } 
        if (verbose) cat("done adjusting", sym, "\n")
    }
    if (auto.assign) return(symout) 
    else return(x)
}

#' get dividend or split data
#'
#' find or download dividend and split data
#'
#' get data that is stored in the 'div' or 'spl' slot of an instrument. If
#' none exists there, it will be downloaded from yahoo.  If the instrument
#' exists, but it did not have a 'div' or 'spl' slot, the newly downloaded
#' data will be stored there.
#' @param Symbol name of instrument
#' @param force if TRUE, data will be downloaded from yahoo even if it already exists
#' in the instrument object.  Downloaded data will overwrite data in instrument object, if any. (FALSE)
#' @param silent silence warning when instrument is not defined (TRUE)
#' @return whatever is in the 'div' or 'spl' slot of the instrument. probably xts data
#' @examples
#' \dontrun{
#' get_div("SPY", silent=FALSE)
#' stock("SPY", currency("USD"))
#' get_div("SPY", silent=FALSE) # will also store in .instrument$SPY$div
#' getInstrument("SPY")$div
#' getInstrument("SPY")$
#' get_div("SPY") # no download...gets data from .instrument envir
#' get_div("SPY", force=TRUE) # downloads from yahoo
#' }
#' @export
#' @rdname get_div
get_div <- function(Symbol, force=FALSE, silent=TRUE) {
    if (!is.instrument(Symbol)) {
        instr <- try(getInstrument(Symbol, silent=TRUE))    
    } else {
        Symbol <- Symbol$primary_id
        instr <- Symbol
    }
    if (!is.instrument(instr)) {
        if (!silent) warning(paste("Could not find instrument.", "Returning dividends data, but not storing."))
        return(getDividends(Symbol))
    }
    if (is.null(instr$div) || force) {
        div <- try(getDividends(Symbol))
        if (!inherits(div, 'try-error')) {
            if (length(div) > 1 && !is.na(div)) colnames(div) <- paste(Symbol, "div", sep=".")            
            instr$div <- div
            assign(Symbol, instr, pos=FinancialInstrument:::.instrument)
        } else return(NULL)
    }
    instr$div    
}

#' @export
#' @rdname get_div
get_spl <- function(Symbol, force=FALSE, silent=TRUE) {
    if (!is.instrument(Symbol)) {
        instr <- try(getInstrument(Symbol, silent=TRUE))    
    } else {
        Symbol <- Symbol$primary_id
        instr <- Symbol
    }
    if (!is.instrument(instr)) {
        if (!silent) warning(paste("Could not find instrument.", "Returning splits data, but not storing."))
        return(getSplits(Symbol))
    }
    if (is.null(instr$spl) || force) {
        spl <- getSplits(Symbol)
        if (!inherits(spl, 'try-error')) {
            if (length(spl) > 1 && !is.na(spl)) colnames(spl) <- paste(Symbol, "spl", sep=".")            
            instr$spl <- spl
            assign(Symbol, instr, pos=FinancialInstrument:::.instrument)
        } else (return(NULL))
    } 
    instr$spl
}



