#' Get the holdings of a Van Eck ETF
#'
#' Download the names and weights of holdings of Van Eck ETFs (Market Vector ETFs)
#'
#' @param Symbols character vector of Van Eck ETF ticker symbols. Presently,
#'   if \code{Symbols} is \code{missing}, all the Market Vectors Symbols found
#'   using \code{\link{read.masterDATA}} will be used.  However, in the future
#'   this may change to require that \code{Symbols} is not \code{missing}
#' @param env environment in which to store holdings data
#' @param auto.assign TRUE/FALSE. If TRUE, the holdings data will be stored in 
#'   an object that has a name that is he Symbol appended with \dQuote{.h}
#' @return either the names of the objects that contain the holdings if called
#'   with \code{auto.assign=TRUE}, or a list of the holdings data.  The returned 
#'   data will be in objects classed as \code{holdings} that are data.frames 
#'   with Weights (0-100) in the first column, and the Names of the stocks in 
#'   the second column. The rownames are the the ticker symbols.
#' @author Garrett See
#' @seealso \code{\link{getHoldings}}, \code{\link{getHoldings.SPDR}}, 
#'   \code{\link{getHoldings.iShares}}, \code{\link{getHoldings.powershares}}
#' @references \url{http://www.vaneck.com/}
#' @examples
#' \dontrun{
#' getHoldings.vaneck("GDX")
#' }
#' @export
getHoldings.vaneck <- function(Symbols, env=.GlobalEnv, auto.assign=TRUE) {
    require("XML")
    if (missing(Symbols)) {
        etfs <- read.masterDATA()
        etfs[, 1] <- sapply(strsplit(etfs[, 1], " "), "[", 1)
        Symbols <- etfs[etfs[["Name"]] == "Market", "Symbol"] #Market Vectors
    }
    if (length(Symbols) == 0L) { return(NULL) }
    hlist <- lapply(Symbols, function(Symbol) {
        if (length(Symbols) > 1) {
            message(paste("Getting holdings for", Symbol))
        }

        tmp <- tempfile()
        download.file(paste0("http://www.vaneck.com/FundHoldings.aspx?ticker=", 
                             Symbol), destfile=tmp, quiet=TRUE)
        tbl <- try(readHTMLTable(tmp, skip.rows=1, header=TRUE, 
                    stringsAsFactors=FALSE)[[1L]], silent=TRUE)
        unlink(tmp)
        if (inherits(tbl, 'try-error')) { return(NULL) }
        if (tbl[1, 1] == "Number") {
            colnames(tbl) <- paste(tbl[1, ])
            tbl <- tbl[-1, ]
        }
        if (!any(grepl("WEIGHT|% of net assets", colnames(tbl)))) {
            tbl$WEIGHT <- NA
        }
        out <- data.frame(as.numeric(gsub("%", "", 
                          tbl[, grep("WEIGHT|% of net assets", colnames(tbl))])),
                          tbl[, grep("Holding|Fund", colnames(tbl))], 
                          stringsAsFactors=FALSE)
        colnames(out) <- c(paste(Symbol, "Weight", sep="."), "Name")
        rn <- if (any(grepl("Ticker", colnames(tbl)))) {
            sapply(strsplit(tbl[["Ticker"]], " "), "[", 1)
        } else gsub("[^A-Za-z0-9]", "", tbl[[grep("CUSIP|Bond Identifier", 
                    names(tbl))]])
        rn[is.na(rn)] <- "--"
        rownames(out) <- make.names(rn, unique=TRUE)
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

