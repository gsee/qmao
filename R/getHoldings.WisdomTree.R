
#WTSymbols()
WTSymbols <- function() {
    URL <- getURL("http://www.wisdomtree.com/etfs/index.aspx")
    ss <- strsplit(URL, "etfid=")[[1L]][-1]
    gsub("(\\d+)\"><span class=\"ticker\">(\\w+)(<.*)", "\\2", 
        ss[grep("<span class=\"ticker\">", ss)])
}

WTIds <- function() {
    URL <- getURL("http://www.wisdomtree.com/etfs/index.aspx")
    ss <- strsplit(URL, "etfid=")[[1L]][-1]
    ss2 <- ss[grep("<span class=\"ticker\">", ss)]
    ids <- gsub("(\\d+)\"><span class=\"ticker\">(\\w+)(<.*)", "\\1", ss2)
    tickers <- gsub("(\\d+)\"><span class=\"ticker\">(\\w+)(<.*)", "\\2", ss2)
    names(ids) <- tickers
    ids
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
#' \code{data.frame} with columns for holdings' weights, names, and symbols.  
#' If called with \code{auto.assign=TRUE}, it will be assigned in \code{env} 
#' with names that are \code{Symbols} appended with \dQuote{.h}.  Otherwise, the 
#' \dQuote{holdings} will be returned and not stored.
#' @author Garrett See
#' @note This does not support Alternative Investment ETFs which, as of this 
#' writing (2012-07), are \dQuote{WDTI} and \dQuote{RRF}.  Wisdom Tree provides
#' these holdings data in the form of Excel spreadsheets and a POST is required
#' to download them.
#'
#' WisdomTree provides more columns than this function returns, but some of 
#' their files have multiple sections with differing numbers of columns.  Also,
#' they have different numbers of columns for different ETFs.  So, at least for 
#' now, only 3 columns are returned with names "Weight", "Name", and "Symbol"
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

    processTable <- function(tbl, hURL, symbol) {
        # this function was extracted from the lapply below.  Some holdings 
        # files have multiple sections, and this function can be applied to each
        if (!"Security.Description" %in% names(tbl)) {
            stop(paste("Column names have changed in csv",
                       "(no Security.Description). Check", hURL))
        }
        if (!"Security.Ticker" %in% names(tbl)) {
            cs <- grep("CUSIP|SEDOL", colnames(tbl))
            if (length(cs) > 0L) {
                names(tbl)[cs[1]] <- "Security.Ticker"
            } else stop(paste("Column names have changed in csv",
                              "(no Security.Ticker). Check", hURL))
        }
        if (!any(grepl("Weight", colnames(tbl)))) tbl$Weight <- NA

        names(tbl)[match("Security.Ticker", names(tbl))] <- "Symbol"
        names(tbl)[match("Security.Description", names(tbl))] <- "Name"
        names(tbl) <- gsub("\\.+$", "", names(tbl)) # remove trailing dot so we don't have "Weight.."

        # split up into 2 groups: 
        # (1) Equities (well, really just things that don't have 
        #     expirations/maturities in the Name)
        # (2) everything else (Fixed Income includes Date like 07/12/12, 
        #     or 09/06/2012)
        
        ## Assume a holding is an equity if it does not have a (maturity) date 
        ## in the Name
        is.eq <- !grepl("\\d+/\\d+/\\d+", tbl$Name)

        eqout <- bout <- NULL
        if (any(is.eq)) {
            eq <- tbl[is.eq, ]
            eqout <- eq[, c("Weight", "Name", "Symbol")]
            rownames(eqout) <- make.names(eq$Symbol, unique=TRUE)
            colnames(eqout)[1] <- paste(symbol, "Weight", sep=".")
            ## check to see if any holdings' symbols are duplicated; if so, add 
            ## a duplicates attr
            dupes <- character(0)
            if (any(duplicated(na.omit(eqout$Symbol)))) {
                dupes <- with(eqout, Symbol[duplicated(Symbol)])
                
                warning(paste(symbol, 
                              "has some holdings with duplicate Symbols:",
                              paste(dupes, collapse=" ")))
            }
            if (length(dupes) > 0) attr(eqout, "duplicates") <- dupes
        }
        if (any(!is.eq)) { #Fixed Income, Repos, Forwards, etc.
            bout <- tbl[!is.eq, c("Weight", "Name", "Symbol")]
            colnames(bout)[1] <- paste(symbol, "Weight", sep=".")
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
        out
    }

    processMultiPartFile <- function(tmpfile, hURL, symbol) {
        # extract sections, process, rbind
        lines <- gsub("^\\s+|\\s+$", "", readLines(tmpfile))
        # find rows that are not holdings data which may be either an empty row or the
        # name of a category of holdings ("Equities", "Currency Contracts", etc.)
        # remove leading and trailing spaces
        #tmp <- gsub("^\\s+|\\s+$", "", grep(",", lines, invert=TRUE, value=TRUE))
        tmp <- grep(",", lines, invert=TRUE, value=TRUE)
        categories <- tmp[nchar(tmp) > 0L]
        if (length(categories) > 0L) {
            sp <- vapply(categories, grep, lines, FUN.VALUE=1L) # start points 
            ep <- c(sp[-1], length(lines) + 1) - 1              # end points

            do.call(rbind, lapply(seq_along(sp), function(i) {
                raw <- lines[sp[i]:ep[i]]
                processTable(read.csv(text=grep(",", raw, value=TRUE)), hURL, 
                             symbol) # only reading lines that contain commas
            }))
        } else {
            tryCatch(read.csv(text=grep(",", lines, value=TRUE)), error=function(e) {
                stop(paste("Cannot read holdings file. Check", hURL))
            })
        }
    }

    hlist <- lapply(seq_along(Symbols), function(i) {
        symbol <- Symbols[i]
        if (i > 1) {
            message(paste("Getting holdings for", symbol))
        }
        id <- names(symbol)
        ## Download holdings into data.frame.
        hURL <- paste0("http://www.wisdomtree.com/etfs/export-holdings.aspx?",
                       "id=", id)
        tmpfile <- tempfile()
        download.file(hURL, destfile=tmpfile, quiet=TRUE)

        tbl <- try(read.csv(tmpfile, stringsAsFactors=FALSE), silent=TRUE)
        if (!inherits(tbl, "try-error")) {
            if (grepl("not.found", colnames(tbl)[1])) {
                # file not found. Bad ticker?
                warning(paste("Error downloading holdings of WisdomTree ETF", 
                              sQuote(symbol)))
                return(NULL)
            }
            out <- processTable(tbl, hURL, symbol)
        } else out <- processMultiPartFile(tmpfile, hURL, symbol)

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

