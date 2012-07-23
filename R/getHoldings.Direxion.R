DirexionSymbols <- function() {
    unique(read.csv("http://www.direxionshares.com/holdings_all.csv", 
                    stringsAsFactors=FALSE)[["Account.Ticker"]])
}

#' Get the holdings of Direxion ETFs
#' 
#' Get the names and weights of Direxion ETFs holdings.
#'
#' This function is usually called by \code{\link{getHoldings}}, but it can also
#' be called directly.
#' 
#' @param Symbols chr Symbols of ETFs (if not supplied, the symbols of all 
#'   Direxion ETFs will be used.)
#' @param env environment in which to store the holdings data
#' @param auto.assign assign data?
#' @return An object classed as \dQuote{holdings} will be created that is a 
#' \code{data.frame} with columns for holdings' weights and names.  If called 
#' with \code{auto.assign=TRUE}, it will be assigned in \code{env} with names 
#' that are \code{Symbols} appended with \dQuote{.h}.  Otherwise, the 
#' \dQuote{holdings} will be returned and not stored.
#' @author Garrett See
#' @seealso \code{\link{getHoldings}}, \code{qmao:::DirexionSymbols}
#' @references \url{http://www.direxionshares.com/etfs}
#' @examples
#' \dontrun{
#' getHoldings.Direxion("FAS")
#' FAS.h
#' }
#' @export
getHoldings.Direxion <- function(Symbols, env=.GlobalEnv, auto.assign=TRUE) {
    s <- DirexionSymbols() # this downloads all Direxion symbols
    Symbols <- if (missing(Symbols)) { s } else Symbols[Symbols %in% s]
    if (length(Symbols) == 0L) { return(NULL) }
    hlist <- lapply(Symbols, function(Symbol) {
        if (length(Symbols) > 1) {
            message(paste("Getting holdings for", Symbol))
        }
        ## Download holdings into data.frame.
        URL <- paste("http://www.direxionshares.com/holdings_", tolower(Symbol), 
                     ".csv", sep="")
        dat <- read.csv(URL, stringsAsFactors=FALSE)
        if (inherits(dat, 'try-error')) { return(NULL) }
        # Calc weights from Market.Value column
        mv.col <- grep("Market\\.Value", names(dat))
        nm.col <- grep("Description", names(dat))
        out <- data.frame(100 * dat[[mv.col]] / sum(dat[[mv.col]]),
                          Name = dat[[nm.col]],
                          dat[, -c(mv.col, nm.col)],
                          row.names = make.names(dat[["Stock.Ticker"]], 
                                                 unique=TRUE))
        colnames(out)[1] <- paste(Symbol, "Weight", sep=".")
        out[order(out[, 1], decreasing=TRUE), ]
        
        ## check to see if any holdings' symbols are duplicated; if so, add a 
        ## duplicates attr
        dupes <- character(0)
        if (any(duplicated(dat[, "Stock.Ticker"]))) {
            dupes <- dat[, "Stock.Ticker"][duplicated(dat[, "Stock.Ticker"])]
            dupes <- dupes[!is.na(dupes)]
            dupes <- dupes[!dupes %in% ""]
            if (length(dupes) > 0L) {
                warning(paste(Symbol, 
                              "has some holdings with duplicate Symbols:", 
                              paste(dupes, collapse=" ")))
            }
        } 
        if (length(dupes) > 0) attr(out, "duplicates") <- dupes
        ## class as holdings, data.frame.
        class(out) <- c("holdings", "data.frame")
        out
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


