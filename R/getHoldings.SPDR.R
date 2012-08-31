
#' Get the ticker symbols of all SPDR ETFs
#' 
#' Get a character vector of the ticker symbols of all SPDR ETFs.
#' @return a character vector of the ticker symbols of all SPDR ETFs.
#' @references \url{https://www.spdrs.com/product}
#' @author Garrett See
#' @seealso getHoldings.SPDR
#' @examples
#' \dontrun{
#' SPDRSymbols()
#' }
SPDRSymbols <- function() {
    sapply(strsplit(strsplit(getURL("https://www.spdrs.com/product/"), 
                                "ticker=")[[1L]], "\">"), "[", 1L)[-1L]
}

#' Get names and weights of the holdings of SPDR ETFs
#'
#' On Non-Unix-alike platforms, this will call 
#' \code{\link{getHoldings.selectSPDR}} which is platform independent 
#' (but only works for the 9 Select Sector SPDRs).
#'
#' This function is usually called by \code{\link{getHoldings}}, but it can also
#' be called directly
#'
#' As of July 8, 2012, the CSVs provided by spdrs.com no longer include a Ticker
#' column which means that the returned \code{holdings} object will no longer 
#' have the ticker symbols of the holdings as rownames.
#'
#' Beware that the CSV that spdrs.com provides for some ETFs (e.g. DIA) has the
#' same number of shares for all holdings (which is incorrect).
#'
#' @param Symbols character vector of SPDR ETF symbols.  Presently, if 
#'   no \code{Symbols} are provided, all SPDR symbols will be used.  However,
#'   in the future it may be changed to require that \code{Symbols} is not
#'   \code{missing}.
#' @param env where to store holdings (only used if \code{auto.assign} is 
#'   \code{TRUE}
#' @param auto.assign should the results be assigned in \code{env}?
#' @return if \code{auto.assign} is TRUE, holdings will be assigned as 
#'   the ETF symbols appended with \dQuote{.h}, and the names of those objects
#'   will be returned. Otherwise, if \code{Symbols} is only one symbol, its
#'   holdings will be returned.  If \code{Symbols} is of length greater than
#'   one, a list will be returned where each element is the holdings of a
#'   different ETF.  If there are no holdings found for a Symbol (most likely 
#'   because it is not a SPDR ETF), and \code{auto.assign} is TRUE, nothing will
#'   be assigned for that Symbol, but if \code{auto.assign} is FALSE, the 
#'   returned list will have \code{NULL} for the element corresponding to that
#'   Symbol.
#' @author Garrett See
#' @note this uses \code{download.file} with \code{method="curl"} which is not
#'   supported on Windows.
#' @seealso \code{\link{getHoldings}}, 
#'   \code{\link{getHoldings.iShares}}, \code{\link{getHoldings.selectSPDR}},
#'   \code{\link{getHoldings.vaneck}}, \code{\link{getHoldings.powershares}},
#'   \code{\link{getHoldings.GlobalX}}, \code{\link{getHoldings.FirstTrust}}
#' @references \url{https://www.spdrs.com/}
#' @examples
#' \dontrun{
#' getHoldings.SPDR("SPY")
#' SPY.h
#' }
#' @import RCurl
#' @export
getHoldings.SPDR <- function(Symbols, env=.GlobalEnv, auto.assign=TRUE) {
  if (.Platform[["OS.type"]] != "unix") {
    return(getHoldings.selectSPDR(Symbols, env=env, auto.assign=auto.assign))
  }
  s <- SPDRSymbols()
  Symbols <- if (missing(Symbols)) { s } else Symbols[Symbols %in% s]
  if (length(Symbols) == 0L) { return(NULL) }
  hlist <- lapply(Symbols, function(symbol) {
    if (length(Symbols) > 1) {
        message(paste("Getting holdings for", symbol))
    }

    lnk <- paste0("https://www.spdrs.com/site-content/xls/", symbol, 
                  "_All_Holdings.xls?fund=", symbol, "&docname=All+Holdings")

    tmp <- tempfile()
    download.file(lnk, destfile=tmp, method='curl', quiet=TRUE)
    if (substr(readLines(tmp, 1), 1, 9) == "<!DOCTYPE") {
        stop(paste("Error downloading holdings of SPDR ETF", sQuote(symbol)))
    }
    fr <- read.xls(tmp, skip=3, stringsAsFactors=FALSE)
    unlink(tmp)
    if (length(colnames(fr)) == 1L) {
        #return(NULL) # HTTP.404..Page.Not.Found
        stop(paste("Error downloading holdings of SPDR ETF", sQuote(symbol)))
    }
    fr <- fr[, -grep("^X", colnames(fr))]
    fr <- fr[complete.cases(fr), ]

    tcol <- grep("ticker", colnames(fr), ignore.case=TRUE)
    if (length(tcol) == 1L) {
        rownames(fr) <- make.names(fr[, tcol], unique=TRUE) 
        fr <- fr[, -tcol]
    }
    wcol <- grep("weight", colnames(fr), ignore.case=TRUE)
    if (length(wcol) > 0L) { 
        colnames(fr)[wcol] <- paste(symbol, "Weight", sep=".") 
    }
    out <- fr[, unique(c(wcol, seq_len(NCOL(fr))))]
    class(out) <- c("holdings", "data.frame")
    out
  })
  names(hlist) <- Symbols
  if (isTRUE(auto.assign)) {
    lapply(Symbols, function(x) {
      assign(paste(x, "h", sep="."), hlist[[x]], pos=env)
    })
    if (length(Symbols) > 0) {
      return(paste(Symbols, "h", sep="."))
    } else (return(NULL))
  }
  if (length(hlist) > 1) {
    return(hlist)
  } else return(hlist[[1L]])
}

#===============================================================================


#' Get names and weights of the holdings of Select Sector SPDR ETFs
#'
#' This function only works with the 9 Select Sector SPDRs: \sQuote{XLY},
#' \sQuote{XLP}, \sQuote{XLE}, \sQuote{XLF}, \sQuote{XLV}, \sQuote{XLI}, 
#' \sQuote{XLB}, \sQuote{XLK}, \sQuote{XLU}.  
#'
#' It is usually called by \code{\link{getHoldings}}, or 
#' \code{\link{getHoldings.SPDR}} but it can also be called directly.
#'
#' @param Symbols character vector of Select Sector SPDR ETF symbols.  If not
#'   provided, all 9 will be used.  However, in the future it may change to 
#'   require that \code{Symbols} is not \code{missing}.
#' @param env where to store holdings (only used if \code{auto.assign} is 
#'   \code{TRUE}
#' @param auto.assign should the results be assigned in \code{env}?
#' @return For each of the \code{Symbols}, an object 
#'   classed as \dQuote{holdings}.  If \code{auto.assign} is TRUE, holdings will 
#'   be assigned as the ETF symbols appended with \dQuote{.h}, and the names of 
#'   those objects will be returned. Otherwise, if \code{Symbols} is only one 
#'   symbol, its holdings will be returned.  If \code{Symbols} is of length 
#'   greater than one, a list will be returned where each element is the 
#'   holdings of a different ETF.  If there are no holdings found for a Symbol 
#'   (most likely because it is not a SPDR ETF), and \code{auto.assign} is TRUE, 
#'   nothing will be assigned for that Symbol, but if \code{auto.assign} is 
#'   FALSE, the returned list will have \code{NULL} for the element 
#'   corresponding to that Symbol.
#' @author Garrett See
#' @note On non-Unix-like platforms, this function is called by 
#' \code{\link{getHoldings}} since this works cross-platform but 
#' \code{\link{getHoldings.SPDR}} does not.
#' @seealso \code{\link{getHoldings}}, \code{\link{getHoldings.SPDR}},
#'   \code{\link{getHoldings.iShares}}, \code{qmao:::getHoldings.iShares.AsOf}
#'   \code{\link{getHoldings.vaneck}}, \code{\link{getHoldings.powershares}}
#' @references \href{http://www.sectorspdr.com}{Sector SPDR website}
#' @examples
#' \dontrun{
#' getHoldings.selectSPDR("XLE")
#' XLE.h
#' getHoldings.selectSPDR(auto.assign=FALSE) #list of all of them
#' }
#' @export
getHoldings.selectSPDR <- function(Symbols, env=.GlobalEnv, auto.assign=TRUE) {
  ssspdrs <- c("XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLU")
  if (missing(Symbols)) { Symbols <- ssspdrs }
  Symbols <- Symbols[Symbols %in% ssspdrs]
  if (length(Symbols) == 0L) { return(NULL) }
  hlist <- lapply(Symbols, function(symbol) {
    if (length(Symbols) > 1) {
        message(paste("Getting holdings for", symbol))
    }

    lnk <- paste0("http://www.sectorspdr.com/content/?do=indexComposition&",
                  "symbol=", symbol, "&filetype=csv")
    tmp <- tempfile()
    download.file(lnk, destfile=tmp, quiet=TRUE) 
    fr <- read.csv(tmp,sep="\t",stringsAsFactors=FALSE)
    unlink(tmp)
    if (NROW(fr) == 0) { return(NULL) }
    fr <- data.frame(fr[, c(4, 2)], row.names=as.character(fr[,3]),
                     stringsAsFactors=FALSE)
    colnames(fr) <- c(paste(symbol,'Weight',sep='.'), "Name")
    class(fr) <- c("holdings", "data.frame")
    fr
  })
  names(hlist) <- Symbols
  if (isTRUE(auto.assign)) {
    sout <- do.call(c, lapply(Symbols, function(x) {
      if (!is.null(hlist[[x]])) {
        assign(paste(x, "h", sep = "."), hlist[[x]], pos = env)
        x
      }
    }))
    if (length(sout) > 0) {
      return(paste(sout, "h", sep = "."))
    } else return(NULL)
  }
  if (length(hlist) > 1) {
    return(hlist)
  } else return(hlist[[1L]])
}



