#' get the holdings of a Van Eck ETF
#' @param Symbols character vector of Van Eck ETF ticker symbols
#' @param env environment in which to store holdings data
#' @param auto.assign TRUE/FALSE. If TRUE, the holdings data will be stored in 
#'   an object that has a name that is he Symbol appended with \dQuote{.h}
#' @return either the names of the objects that contain the holdings if called
#'   with \code{auto.assign=TRUE}, or the holdings data.  The returned data
#'   is a data.frame with Weights (0-100) in the first column, and the names
#'   of the stocks in the second column. The rownames are the the ticker symbols.
#' @author Garrett See
#' @seealso \code{\link{getHoldings}}
#' @references \url{http://www.vaneck.com/}
#' @note This should become part of \code{getHoldings} (or getHoldings should
#'   become \dQuote{generic}).  
#'   
#' Internally, this uses \code{\link[XML]{readHTMLTable}} to read an Excel
#' spreadsheet because \code{readHTMLTable} is incredibly easy to implement.
#' However, it is a little bit slow.
#' @examples
#' \dontrun{
#' getHoldings.vaneck("GDX")
#' }
#' @export
getHoldings.vaneck <- function(Symbols, env=.GlobalEnv, auto.assign=TRUE) {
    require("XML")
    lout <- lapply(Symbols, function(Symbol) {
        
        tbl <- readHTMLTable(paste("http://www.vaneck.com/FundHoldings.aspx?ticker=", Symbol, sep=""),
                             skip.rows=1, header=TRUE, stringsAsFactors=FALSE)[[1L]]
        out <- data.frame(as.numeric(gsub("%", "", tbl[, "% of net assets"])),
                          tbl[, "Holding"], stringsAsFactors=FALSE)
        colnames(out) <- c(paste(Symbol, "Weight", sep="."), "Name")
        rn <- sapply(strsplit(tbl[["Ticker"]], " "), "[", 1)
        rn[is.na(rn)] <- "--"
        rownames(out) <- rn
        out[rownames(out) != '--', ] # to match qmao:::getHoldings behavior
    })
    if (isTRUE(auto.assign)) {
        lapply(seq_along(Symbols), function(i) {
            assign(paste(Symbols[i], "h", sep="."), lout[[i]], pos=env)
        })
        return(paste(Symbols, "h", sep="."))
    }
    if (length(lout) > 1) return(lout)
    lout[[1]]
}
