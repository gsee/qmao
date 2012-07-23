
#' download CSVs from PowerShares.com
#' @param base.url the non-product-specific url
#' @param event.target This is the first string inside of the 
#'   javascript:__doPostBack() function that you will get when you right click 
#'   the \dQuote{Download} link and \dQuote{Copy Link Address}
#' @param action the product-specific part of the action url
#' @return a csv downloaded from the PowerShares website
#' @author Garrett See with help from ttmaccer at 
#'   \url{http://stackoverflow.com/a/11004901}
#' @references \url{http://www.invescopowershares.com/products/}
dlPowerShares <- function(base.url = "http://www.invescopowershares.com/products/",
                          event.target = 'ctl00$FullPageOverrideContent$LinkButton1',
                          action = "") {
    stopifnot(require("RCurl"))
    # The following code is copied with slight modifications from the answer
    # provided by ttmaccer at http://stackoverflow.com/a/11004901
    ftarget <- paste0(base.url, action)
    dum <- getURL(ftarget)
    event.val <- unlist(strsplit(dum,"__EVENTVALIDATION\" value=\""))[2]
    event.val <- unlist(strsplit(event.val,"\" />\r\n\r\n<script"))[1]
    view.state <- unlist(strsplit(dum,"id=\"__VIEWSTATE\" value=\""))[2]
    view.state <- unlist(strsplit(view.state,"\" />\r\n\r\n\r\n<script"))[1]
    web.data <- postForm(ftarget, "form name" = "aspnetForm", 
                         "method" = "POST", 
                         "action" = action, 
                         "id" = "aspnetForm",
                         "__EVENTTARGET"=event.target,
                         "__EVENTVALIDATION"=event.val,
                         "__VIEWSTATE"=view.state)
    read.csv(text=web.data[1], stringsAsFactors=FALSE)
}


#' Get the names and weights of the holdings of PowerShares ETFs
#' 
#' Download the holdings of PowerShares ETFs and return a data.frame containing
#' the weights and names of the holdings (among other things).
#' 
#' This function is usually called by \code{\link{getHoldings}}, but it can also
#' be called directly.
#' 
#' First, this will download a list of PowerShares ETFs.  If \code{Symbols} is
#' missing, the holdings of all PowerShares ETFs will be retrieved.  Otherwise,
#' only the \code{Symbols} that are in the list of PowerShares ETFs will be 
#' used.  Powershares does NOT provide holdings details about 
#' \sQuote{Commodity & Currency} ETFs, and they provide different columns 
#' depending on the type of ETF (i.e. equity, preferred stock, fixed income, 
#' etc.).  Although different columns may be returned depending on which ETF
#' you request, the first two columns will always be the same: Weight (1-100), 
#' and Name.
#' 
#' This will return a data.frame with a row for each holding of the ETF.  If 
#' the holdings are stocks, the rownames will be the stocks' ticker symbols.  
#' Otherwise, if there is a column called \dQuote{SecurityNum} (which all of 
#' them presently have), the rownames will become \code{make.names(SecurityNum, 
#' unique=TRUE)}.  Finally, if there is no \dQuote{SecurityNum} column the 
#' rows will not be named.
#' 
#' @param Symbols character vector of PowerShares ETF symbols.  Presently, if
#'   Symbols is not provided, then the symbols of all PowerShares that have
#'   holdings data will be used.  However, in the future it may change to 
#'   require that the user provide a value.
#' @param env where to store holdings
#' @param auto.assign should the results be assigned in \code{env}?
#' @return if \code{auto.assign} is TRUE, holdings will be assigned as 
#'   the ETF symbols appended with \dQuote{.h}, and the names of those objects
#'   will be returned. Otherwise, if \code{Symbols} is only one symbol, its
#'   holdings will be returned.  If \code{Symbols} is of length greater than
#'   one, a list will be returned where each element is the holdings of a
#'   different ETF.
#' @author Garrett See with help from ttmaccer at 
#'   \url{http://stackoverflow.com/a/11004901}
#' @references \url{http://www.invescopowershares.com}
#' @seealso \code{\link{getHoldings}}, 
#'   \code{qmao:::getHoldings.iShares.AsOf}
#'   \code{\link{getHoldings.SPDR}}
#'   \code{\link{getHoldings.vaneck}}
#' @examples
#' \dontrun{
#' getHoldings.powershares(c("QQQ", "PGX"))
#' head(QQQ.h); head(PGX.h)
#' }
#' @export
getHoldings.powershares <- function(Symbols, env=.GlobalEnv, auto.assign=TRUE) {
    stopifnot(require("RCurl"))
    pspl <- dlPowerShares() #PowerShares Product list
    ps.syms <- pspl[pspl[["Category_Name"]] != "Commodity & Currency", "Ticker"]
    if (missing(Symbols)) {
        Symbols <- ps.syms
    } else Symbols <- Symbols[Symbols %in% ps.syms]
    if (length(Symbols) == 0L) { return(NULL) }
    hlist <- lapply(Symbols, function(Symbol) {
        if (length(Symbols) > 1) {
            message(paste("Getting holdings for", Symbol))
        }
        dat <- dlPowerShares(
            event.target=paste0("ctl00$MainPageLeft$MainPageContent$", 
                                "ExportHoldings1$LinkButton1"),
            action=paste0("holdings.aspx?ticker=", Symbol))    
        wt.col <- grep("Weight|PercentageOfFund", names(dat), ignore.case=TRUE)
        name.col <- grep("Name", names(dat), ignore.case=TRUE)
        out <- dat[, c(wt.col, name.col, seq_along(dat)[-c(wt.col, name.col)])]
        # Name the 1st 2 columns like other getHoldings functions
        colnames(out)[1:2] <- paste(c(paste(Symbol, "Weight", sep="."), "Name"))
        
        if (any(grepl("HoldingsTicker", names(out), ignore.case=TRUE))) {
            rownames(out) <- make.names(out[[grep("HoldingsTicker", names(out), 
                                                  ignore.case=TRUE)]], 
                                        unique=TRUE)
        } else if (any(grepl("SecurityNum", names(out), ignore.case=TRUE))) {
            rownames(out) <- make.names(out[[grep("SecurityNum", names(out), 
                                                  ignore.case=TRUE)]], 
                                        unique=TRUE)
        }
        # to be consistent with other getHoldings functions, weights should 
        # sum to 100 (not 1)
        out[[1L]] <- out[[1L]] * 100
        #remove some redundant info
        omit <- grep("ticker", names(out), ignore.case=TRUE) 
        omit <- omit[omit > 2]
        if (length(omit) > 0) {
            out <- out[, -omit]
        }
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
        if (length(sout) > 0) {
            return(paste(sout, "h", sep="."))
        } else return(NULL)
    }
    if (length(hlist) > 1) {
        return(hlist)
    } else return(hlist[[1]])
}

