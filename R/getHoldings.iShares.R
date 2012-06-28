#' Get the holdings of iShares ETFs
#' 
#' Get the names and weights of iShares ETFs.
#' 
#' Some iShares ETFs have more than one holding listed with the same Symbol.
#' In the case that Symbols are duplicated, a \dQuote{duplicates} attribute
#' will be added to the returned object.  Also, since rownames cannot be 
#' duplicated, they will be made unique with \code{\link{make.unique}}
#' 
#' @param Symbols chr Symbols of ETFs (if not supplied, the symbols of all 
#'   iShares will be used, which could take a long time since there are over 
#'   200 of them.)
#' @param env environment to store the holdings data in
#' @param auto.assign assign data?
#' @return An object classed as \dQuote{weights} will be created that is a 
#' data.frame with columns for holdings' weights and names.  If called with 
#' \code{auto.assign=TRUE}, it will be assigned in \code{env} with names that 
#' are \code{Symbols} appended with \dQuote{.h}.  Otherwise, the 
#' \dQuote{weights} will be returned and not stored.
#' @author Garrett See
#' @references Sector SPDRs \href{www.ishares.com}{iShares}
#' @examples
#' \dontrun{
#' getHoldings.iShares('IVE')
#' IVE.h
#' }
#' @export
getHoldings.iShares <- function(Symbols, env=.GlobalEnv, auto.assign=TRUE) {
  tmp <- tempfile()
  download.file("http://us.ishares.com/product_info/fund/excel_profile.htm",
                destfile=tmp)
  ishr.syms <- as.character(read.csv(tmp, stringsAsFactors=FALSE, 
                                     header=FALSE, skip=3)[, 2])
  unlink(tmp)
  if (missing(Symbols)) { Symbols <- ishr.syms }
  Symbols <- Symbols[Symbols %in% ishr.syms]
  if (length(Symbols) == 0L) { return(NULL) }
  hlist <- lapply(Symbols, function(symbol) {
    if (length(Symbols) > 1) {
        message(paste("Getting holdings for", symbol))
    }
    lnk <- paste0('http://us.ishares.com/product_info/fund/excel_holdings.htm?',
                  'ticker=', symbol, sep="")
    tmp <- tempfile()
    download.file(lnk, destfile=tmp)
    fr <- try(read.csv(tmp, skip=11, stringsAsFactors=FALSE), silent=TRUE)
    if (inherits(fr, 'try-error')) {
      fr <- try(read.csv(tmp, skip=16, stringsAsFactors=FALSE, header=FALSE), 
                silent=TRUE)
      colnames(fr) <- c("Symbol", fr[1, -NCOL(fr)])
      fr <- fr[-1, ]
    }
    unlink(tmp)
    fr <- fr[1:(length(fr[,1])-3), ]
    fr[, 1] <- gsub(" ", "", fr[, 1])
    dupes <- character(0)
    if (any(duplicated(fr[, 1])) && !all(fr[, 1] == "--")) {
      dupes <- fr[, 1][duplicated(fr[, 1])]
      warning(paste("Some Symbols duplicated:", paste(dupes, collapse=" ")))
    }
    rownames(fr) <- make.names(fr[, 1], unique=TRUE)
    wcol <- grep("Net.Assets", colnames(fr))
    if (wcol != 3) { 
      warning(paste0("The format of the spreadsheet has changed",
                     " since this function was written!")) 
    }
    fr[, 1] <- fr[, wcol]
    colnames(fr)[1] <- paste(symbol,'Weight',sep='.')
    fr <- fr[, -wcol]
    if (length(dupes) > 0) attr(fr, "duplicates") <- dupes
    class(fr) <- c("weights", "data.frame")
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

## get/check the column names of all iShares
#cout <- lapply(ishr.syms, function(.x) {
#    out <- try({
#        tmp <- tempfile()
#        download.file(paste('http://us.ishares.com/product_info/fund/excel_holdings.htm?ticker=',.x,sep=""), destfile=tmp)
#        fr <- read.csv(tmp, skip=11, stringsAsFactors=FALSE)
#        unlink(tmp)
#        colnames(fr)
#    })
#    if (!inherits(out, 'try-error')) out
#})
#unique(cout)
#[[1]]
# [1] "CUSIP"              "Name"               "X..Net.Assets"      "Market.Value"       "Maturity"          
# [6] "Effective.Duration" "Coupon"             "Yield.to.Maturity"  "Yield.to.Worst"     "Sector"            
#[11] "Market.Price"       "Par.Value"          "Next.Call.Date"     "Next.Call.Price"   
#
#[[2]]
# [1] "Symbol"        "Name"          "X..Net.Assets" "Market.Value"  "ISIN"          "Sedol"         "Market"       
# [8] "Sector"        "Exchange.Rate" "Market.Price"  "Shares.Held"  
#
#[[3]]
# [1] "ISIN"               "Name"               "X..Net.Assets"      "Market.Value"       "Maturity"          
# [6] "Effective.Duration" "Coupon"             "Yield.to.Maturity"  "Yield.to.Worst"     "Sector"            
#[11] "Market.Price"       "Par.Value"          "Next.Call.Date"     "Next.Call.Price"   
#
#[[4]]
# [1] "ISIN"                   "Name"                   "X..Net.Assets"          "Market.Value"          
# [5] "Maturity"               "Real.Yield.Duration"    "Coupon"                 "Real.Yield.To.Maturity"
# [9] "Yield.to.Worst"         "Sector"                 "Market.Price"           "Par.Value"             
#[13] "Next.Call.Date"         "Next.Call.Price"       
#
#[[5]]
# [1] "HoldingsSymbol" "Name"           "X..Net.Assets"  "Market.Value"   "ISIN"           "Sedol"         
# [7] "Market"         "Sector"         "Exchange.Rate"  "Market.Price"   "Shares.Held"   

