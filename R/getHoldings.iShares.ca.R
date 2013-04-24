#' Get the holdings of Canadian iShares ETFs
#' 
#' Get the names and weights of Canadian iShares ETFs.
#'
#' This function can be called by \code{\link{getHoldings}}, or it can
#' be called directly
#' 
#' Some ETFs have more than one holding listed with the same Symbol.
#' In the case that Symbols are duplicated, a \dQuote{duplicates} attribute
#' will be added to the returned object.  Also, since rownames cannot be 
#' duplicated, they will be made unique with \code{\link{make.unique}}
#' 
#' @param Symbols chr Symbols of ETFs (if not supplied, the symbols of all 
#'   Canadian iShares will be used)
#' @param env environment to store the holdings data in
#' @param auto.assign assign data?
#' @return An object classed as \dQuote{holdings} will be created that is a 
#' data.frame with columns for holdings' weights and names.  If called with 
#' \code{auto.assign=TRUE}, it will be assigned in \code{env} with names that 
#' are \code{Symbols} appended with \dQuote{.h}.  Otherwise, the 
#' \dQuote{holdings} will be returned and not stored.
#' @author Garrett See
#' @seealso \code{\link{getHoldings}}
#' @references \url{www.ishares.com}#' @examples
#' \dontrun{
#' getHoldings.iShares.ca("XIU")
#' XIU.h
#' }
#' @export
getHoldings.iShares.ca <- function(Symbols, env=.GlobalEnv, auto.assign=TRUE) {
  Symbols <- sub("\\.TO$", "", Symbols) # strip .TO from the end of tickers if they have it
  
  tmp <- tempfile()
  suppressWarnings(download.file("http://ca.ishares.com/product_info/fund/excel_returns.htm",
                                 destfile=tmp, quiet=TRUE))
  
  ishr.syms <- as.character(read.csv(tmp, stringsAsFactors=FALSE, header=FALSE, skip=4)[, 2])
  unlink(tmp)
  # There are a couple symbols with inconvenient holdings files; for now, they're being ignored.
  if (missing(Symbols)) { Symbols <- ishr.syms[!ishr.syms %in% c("ALT", "GSG")] }
  Symbols <- Symbols[Symbols %in% ishr.syms]
  if (length(Symbols) == 0L) { return(NULL) }
  hlist <- lapply(Symbols, function(symbol) {
    if (length(Symbols) > 1) {
      message(paste("Getting holdings for", symbol))
    }
    #lnk <- paste0("http://ca.ishares.com/product_info/fund/excel_holdings.htm?ticker=XGD&periodCd=d")
    lnk <- paste0("http://ca.ishares.com/product_info/fund/excel_holdings.htm?",
                  "ticker=", symbol)
    tmp <- tempfile()
    download.file(lnk, destfile=tmp, quiet=TRUE)
    fr <- readHTMLTable(tmp, stringsAsFactors=FALSE)[[2]]
    unlink(tmp)
    fr[, 1] <- gsub(" ", "", fr[, 1])
    dupes <- character(0)
    if (any(duplicated(fr[, 1])) && !all(fr[, 1] == "--")) {
      dupes <- unique(fr[, 1][duplicated(fr[, 1])])
      dupes <- dupes[dupes != "--"]
      if (length(dupes) > 0L)
        warning(paste(symbol, "has some holdings with duplicate Symbols:", 
                      paste(dupes, collapse=" ")))
    }
    rownames(fr) <- make.names(fr[, 1], unique=TRUE)
    colnames(fr) <- make.names(colnames(fr))
    wcol <- grep("Net.Assets", colnames(fr), fixed=TRUE)
    if (length(wcol) == 0) { 
      stop(paste0("Cannot find 'Net.Assets' column. ",
                  "The format of the spreadsheet may have changed",
                  " since this function was written!")) 
    }
    fr[, 1] <- fr[, wcol]
    colnames(fr)[1] <- paste(symbol,'Weight',sep='.')
    fr <- fr[, -wcol]
    if (length(dupes) > 0) attr(fr, "duplicates") <- dupes
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
    return(paste(sout, "h", sep = "."))
  }
  if (length(hlist) > 1) {
    return(hlist)
  } else return(hlist[[1L]])
}
