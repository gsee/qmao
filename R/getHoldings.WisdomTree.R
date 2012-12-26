
#WTSymbols()
WTSymbols <- function() {
    URL <- getURL("http://www.wisdomtree.com/etfs/index.aspx")
    ss <- strsplit(URL, "etfid=")[[1L]][-1]
    gsub("(\\d+)\"><span class=\"ticker\">(\\w+)(<.*)", "\\2", 
        ss[grep("<span class=\"ticker\">", ss)])
}


#' Get the holdings of Wisdom Tree ETFs
#' 
#' Get the names and weights of holdings of Wisdom Tree ETFs.
#'
#' This function is usually called by \code{\link{getHoldings}}, but it can also
#' be called directly
#' 
#' @param Symbols chr Symbols of ETFs (if not supplied, the symbols of all 
#'   Wisdom Tree ETFs will be used.)
#' @param env environment in which to store the holdings data
#' @param auto.assign assign data?
#' @return An object classed as \dQuote{holdings} will be created that is a 
#' \code{data.frame} with columns for holdings' weights and names.  If called 
#' with \code{auto.assign=TRUE}, it will be assigned in \code{env} with names 
#' that are \code{Symbols} appended with \dQuote{.h}.  Otherwise, the 
#' \dQuote{holdings} will be returned and not stored.
#' @author Garrett See
#' @note This does not support Alternative Investment ETFs which, as of this 
#' writing (2012-07), are \dQuote{WDTI} and \dQuote{RRF}.  Wisdom Tree provides
#' these holdings data in the form of Excel spreadsheets and a POST is required
#' to download them.
#' @seealso \code{\link{getHoldings}}
#' @references \url{http://www.wisdomtree.com/etfs/}
#' @examples
#' \dontrun{
#' getHoldings.WisdomTree('EU')
#' EU.h
#' getHoldings.WisdomTree('ICN', auto.assign=FALSE)
#' getHoldings.WisdomTree('DTN', auto.assign=FALSE)
#' }
#' @export
getHoldings.WisdomTree <- function(Symbols, env=.GlobalEnv, auto.assign=TRUE) {
    URL <- getURL("http://www.wisdomtree.com/etfs/index.aspx")
    #s <- gsub("^(\\w+)(<.*)", "\\1", strsplit(URL, "\"ticker\">")[[1L]][-1])
    #ids <- unique(gsub("^(\\w+)\\s?(.*)$", "\\1", 
    #                   strsplit(URL, "etfid=")[[1L]][-1]))
    ss <- strsplit(URL, "etfid=")[[1L]][-1]
    ss2 <- ss[grep("<span class=\"ticker\">", ss)]
    ids <- gsub("(\\d+)\"><span class=\"ticker\">(\\w+)(<.*)", "\\1", ss2)
    tickers <- gsub("(\\d+)\"><span class=\"ticker\">(\\w+)(<.*)", "\\2", ss2)
    Symbols <- if (missing(Symbols)) {
        tickers
    } else Symbols[Symbols %in% tickers]
    names(Symbols) <- ids[tickers %in% Symbols]
    # The holdings data for "Alternative ETFs" come in an Excel file. 
    # Not supported yet. (and probably never will be)
    # As of 2012-07-12 there are 2 of these "76" WTDI, and "72" RRF
    #Symbols <- Symbols[!names(Symbols) %in% c("76", "72")]
    Symbols <- Symbols[!unname(Symbols) %in% c("WDTI", "RRF")]
    if (length(Symbols) == 0L) { return(NULL) }
    hlist <- lapply(seq_along(Symbols), function(i) {
        symbol <- Symbols[i]
        if (i > 1) {
            message(paste("Getting holdings for", symbol))
        }
        id <- names(symbol)
        ## Download holdings into data.frame.
        hURL <- paste0("http://www.wisdomtree.com/etfs/fund-holdings.aspx?",
                       "etfid=", id)
        tbl <- try(readHTMLTable(hURL, stringsAsFactors=FALSE)[[1L]])
        if (inherits(tbl, "try-error")) {
          warning(paste("holdings for", symbol, "could not be found."))
          return(NULL)
        }
        # Remove the row numbers that are embedded in text (e.g. "1. ", "2. ")
        tbl[["Name"]] <- gsub("(\\d+\\.\\s)(.*)", "\\2", tbl[["Name"]])
        # split up into 2 groups: 
        # (1) Equities which have a Symbol like (XXX), and
        # (2) everything else (Fixed Income includes Date like 07/12/12, 
        #     or 09/06/2012)
        ## It's an equity if it has an embedded ticker like "(XXX)" and it does
        ## not have an embedded date like "10/2013"
        is.eq <- grepl("\\([A-Z0-9-]+\\)", tbl[["Name"]]) & 
                 !grepl("\\d+/\\d+", tbl[["Name"]])
        eqout <- bout <- NULL
        if (any(is.eq)) {
            eq <- tbl[is.eq, ]
            #Symbol <- gsub("[\\(\\)]", "", gsub("(.*)(\\s)(\\(\\w+\\))", "\\3", 
            Symbol <- gsub("[\\(\\)]", "", gsub("(.*)(\\s)(\\([A-Z0-9-]+\\))", 
                                                "\\3", eq[["Name"]]))
            eqout <- data.frame(eq[["Weight"]], 
                Name=gsub("(.*)(\\s.*)", "\\1", eq[["Name"]]),
                eq[, 2], Symbol, row.names=make.names(Symbol, unique=TRUE), 
                stringsAsFactors=FALSE)
            colnames(eqout) <- c("Weight", "Name", names(eq)[2L], "Symbol")
            ## check to see if any holdings' symbols are duplicated; if so, add 
            ## a duplicates attr
            dupes <- character(0)
            if (any(duplicated(Symbol))) {
                dupes <- Symbol[duplicated(Symbol)]
                warning(paste(symbol, 
                              "has some holdings with duplicate Symbols:",
                              paste(dupes, collapse=" ")))
            }
            if (length(dupes) > 0) attr(eqout, "duplicates") <- dupes
        }
        # Could use something like this to find Fixed Income
        #bonds <- tbl[["Name"]][grep("\\d+/\\d+/\\d+", tbl[["Name"]])]
        if (any(!is.eq)) { #Fixed Income, Repos, Forwards, etc.
            bnd <- tbl[!is.eq, ]
            bout <- data.frame(bnd[["Weight"]], bnd[["Name"]], bnd[, 2], Symbol=NA,
                               stringsAsFactors=FALSE)
            colnames(bout) <- c("Weight", "Name", names(bnd)[2], "Symbol")
        }
        out <- NULL
        if (length(eqout) > 0L) {
            out <- eqout
        }
        if (length(bout) > 0L) {
            if (length(out) > 0L) {
                out <- rbind(out, bout)
            } else out <- bout
        }
        if (any(grepl("(NDF|FWD)(\\s+)(BUY|SELL)(*.)", out$Name))) {
            warning(paste(symbol, 
                       "holds forwards which may not be reflected in 'Weight'"))
        }
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

