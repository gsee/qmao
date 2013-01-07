#' Get the holdings of Global X ETFs
#' 
#' Get the names and weights of holdings of Global X ETFs.
#'
#' This function is usually called by \code{\link{getHoldings}}, but it can also
#' be called directly
#' 
#' @param Symbols chr Symbols of ETFs (if not supplied, the symbols of all 
#'   Global X ETFs that are included in the masterDATA csv
#'   (\url{"http://www.masterdata.com/HelpFiles/ETF_List_Downloads/AllTypes.csv"})
#'   will be used.)
#' @param env environment in which to store the holdings data
#' @param auto.assign assign data?
#' @return An object classed as \dQuote{holdings} will be created that is a 
#' \code{data.frame} with columns for holdings' weights and names.  If called 
#' with \code{auto.assign=TRUE}, it will be assigned in \code{env} with names 
#' that are \code{Symbols} appended with \dQuote{.h}.  Otherwise, the 
#' \dQuote{holdings} will be returned and not stored.
#' @author Garrett See
#' @seealso \code{\link{getHoldings}}
#' @references \url{http://www.globalxfunds.com/}
#' @examples
#' \dontrun{
#' getHoldings.GlobalX('SIL', auto.assign=FALSE)
#' }
#' @export
getHoldings.GlobalX <- function(Symbols, env=.GlobalEnv, auto.assign=TRUE) {
    if (missing(Symbols)) {
        etfs <- read.masterDATA()
        etfs[, 1] <- sapply(strsplit(etfs[, 1], " "), "[", 1)
        Symbols <- etfs[etfs[["Name"]] == "Global", "Symbol"] #Global X
    }
    if (length(Symbols) == 0L) { return(NULL) }
    hlist <- lapply(Symbols, function(Symbol) {
        if (length(Symbols) > 1) {
            message(paste("Getting holdings for", Symbol))
        }

        #html <- getURL("http://www.globalxfunds.com/COPX")
        text <- readLines(paste("http://www.globalxfunds.com", Symbol, sep="/"))
        # sorry for the ugly regex below; I'm sure it could be simpler.  
        # It just finds the numbers after the text "fund_id" from the line that 
        # includes something like 
        # <a href=\"fundholdings_excel.php?fund_id=18492\">DOWNLOAD FULL HOLDINGS</a>
        id <- gsub("^(\\w+)\\s?(.*)$", "\\1", gsub("(.*)(fund_id=)(*.)", "\\3", 
                                               text[grep("fund_id", text)]))
        if (length(id) == 0L) { return(NULL) }
        URL <- paste0("http://www.globalxfunds.com/fundholdings_excel.php?",
                      "fund_id=", id)
        tmpfile <- tempfile()
        download.file(URL, destfile=tmpfile, quiet=TRUE)
        dat <- read.xls(tmpfile, pattern="Name")
        unlink(tmpfile)
        if (inherits(dat, 'try-error')) { return(NULL) }
        dupes <- character(0)
        if (any(duplicated(dat[["Identifier"]]))) {
            dupes <- dat[["Identifier"]][duplicated(dat[["Identifier"]])]
            warning(paste(Symbol, "has some holdings with duplicate Symbols:", 
                          paste(dupes, collapse=" ")))
        }
        if (length(dupes) > 0) attr(dat, "duplicates") <- dupes
        
        # more ugly regex; convert colnames from things like 
        # "X.Market.Value...." to "Market.Value"        
        colnames(dat) <- gsub("\\.$", "", 
                              gsub("X\\.", "", 
                                   gsub("\\.+", "\\.", colnames(dat))))
        colnames(dat)[1] <- paste(Symbol, "Weight", sep=".")
        rownames(dat) <- make.names(dat[["Identifier"]], unique=TRUE)
        class(dat) <- c("holdings", "data.frame")
        dat
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
