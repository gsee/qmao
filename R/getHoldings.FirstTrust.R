#' Get the holdings of First Trust ETFs
#' 
#' Get the names and weights of holdings of First Trust ETFs.
#'
#' This function is usually called by \code{\link{getHoldings}}, but it can also
#' be called directly
#' 
#' @param Symbols chr Symbols of ETFs (if not supplied, the symbols of all 
#'   First Trust ETFs will be used.)
#' @param env environment in which to store the holdings data
#' @param auto.assign assign data?
#' @return An object classed as \dQuote{holdings} will be created that is a 
#' \code{data.frame} with columns for holdings' weights and names.  If called 
#' with \code{auto.assign=TRUE}, it will be assigned in \code{env} with names 
#' that are \code{Symbols} appended with \dQuote{.h}.  Otherwise, the 
#' \dQuote{holdings} will be returned and not stored.
#' @author Garrett See
#' @seealso \code{\link{getHoldings}}
#' @references \url{http://www.ftportfolios.com/index.aspx}
#' @examples
#' \dontrun{
#' getHoldings.FirstTrust('FDN')
#' FDN.h
#' }
#' @export
#tmp <- rHT[grep("ETFList", names(rHT))]
getHoldings.FirstTrust <- function(Symbols, env=.GlobalEnv, auto.assign=TRUE) {
    rHT <- readHTMLTable("http://www.ftportfolios.com/Retail/etf/etflist.aspx", stringsAsFactors=FALSE)
    s <- unique(unlist(lapply(rHT, function(x) {
        x$TickerSymbol
    }), use.names=FALSE))
    Symbols <- if (missing(Symbols)) { s } else Symbols[Symbols %in% s]
    if (length(Symbols) == 0L) { return(NULL) }
    hlist <- lapply(Symbols, function(Symbol) {
        if (length(Symbols) > 1) {
            message(paste("Getting holdings for", Symbol))
        }

        ## Download holdings into data.frame.
        URL <- paste0("http://www.ftportfolios.com/Retail/etf/ETFholdings.aspx",
                     "?Ticker=", Symbol, sep="")
        tbl <- try(readHTMLTable(URL, stringsAsFactors=FALSE))
        if (inherits(tbl, 'try-error')) { return(NULL) }

        dat <- tbl[[grep("HoldingsListing_FundNavigation1", names(tbl)) + 1]]
        colnames(dat) <- dat[1, ]
        dat <- dat[-1, ]
        out <- data.frame(dat[, c(3, 1, 2)], 
                          row.names=make.names(dat[, 2], unique=TRUE))
        colnames(out) <- c(paste(Symbol, "Weight", sep="."), "Name", "Ticker")
        out[, 1] <- as.numeric(sub("%", "", out[, 1]))
        out$Name <- gsub("[^A-Za-z0-9 ,.&()-/]", "", out$Name)
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
