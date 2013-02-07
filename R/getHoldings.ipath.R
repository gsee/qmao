#' Get the holdings of an iPath ETN
#'
#' Download the names and weights of holdings of iPath ETNs
#'
#' @param Symbols character vector of iPath ETN ticker symbols. Presently,
#'   if \code{Symbols} is \code{missing}, all the iPath Symbols found
#'   by scraping \url{http://www.ipathetn.com/us/product_information} will be 
#'   used.  However, in the future this may change to require that 
#'   \code{Symbols} is not \code{missing}
#' @param env environment in which to store holdings data
#' @param auto.assign TRUE/FALSE. If TRUE, the holdings data will be stored in 
#'   an object that has a name that is he Symbol appended with \dQuote{.h}
#' @return either the names of the objects that contain the holdings if called
#'   with \code{auto.assign=TRUE}, or a list of the holdings data.  The returned 
#'   data will be in objects classed as \code{holdings} that are data.frames 
#'   with Weights (0-100) in the first column, and the Names of the holdings in 
#'   the second column. The rownames are the the ticker symbols.
#' @author Garrett See
#' @note these funds are not included in the masterDATA csv used by 
#'   \code{\link{getHoldings}}.  Therefore, \code{getHoldings} \emph{will not} 
#'   dispatch \code{getHoldings.ipath} -- it \emph{must} be called directly.
#' @seealso \code{\link{getHoldings}}, \code{\link{getHoldings.SPDR}}, 
#'   \code{\link{getHoldings.iShares}}, \code{\link{getHoldings.powershares}},
#'   \code{\link{getHoldings.vaneck}}
#' @references \url{http://www.ipathetn.com/}
#' @examples
#' \dontrun{
#' getHoldings.ipath("BCM")
#' }
#' @export
getHoldings.ipath <- function(Symbols, env=.GlobalEnv, auto.assign=TRUE) {
    if (missing(Symbols)) {
      Symbols <- iPathSymbols()
    }
    if (length(Symbols) == 0L) { return(NULL) }
    hlist <- lapply(Symbols, function(Symbol) {
      if (length(Symbols) > 1) {
        message(paste("Getting holdings for", Symbol))
      }
      base.url <- "http://www.ipathetn.com/us/product/index-components-csv/"
      dat <- try(read.csv(paste(base.url, toupper(Symbol), sep=""), 
                          stringsAsFactors=FALSE, header=TRUE, skip=1))
      if (inherits(dat, 'try-error')) { return(NULL) }
      
      ## Find the column that contains the ticker and the column that contains
      ## the weights.  Then put the columns in the order we want.
      tcol <- grep("ticker|symbol", colnames(dat), ignore.case=TRUE)
      if (length(tcol) == 1L) {
        #rownames will be ticker symbols of holdings
        rownames(dat) <- make.names(dat[, tcol], unique=TRUE) 
        #dat <- dat[, -tcol]
      }
      wcol <- grep("weight", colnames(dat), ignore.case=TRUE)
      if (length(wcol) > 0L) { 
        colnames(dat)[wcol] <- paste(Symbol, "Weight", sep=".") 
      }
      out <- dat[, unique(c(wcol, seq_len(NCOL(dat))))]
      colnames(out)[2] <- "Name"
        
      ## check to see if any holdings' symbols are duplicated; if so, add a 
      ## duplicates attr
      dupes <- character(0)
      if (any(duplicated(out[, tcol]))) {
        dupes <- out[, tcol][duplicated(out[, tcol])]
        if (!all(is.na(dupes))) {
          warning(paste(Symbol, "has some holdings with duplicate Symbols:", 
                        paste(dupes, collapse=" ")))
        }
      }
      if (length(dupes) > 0) attr(out, "duplicates") <- dupes
      # The first row has a Descrition of the ETF and the Date of the holdings file
      # Let's add those as attributes
      line1 <- readLines(paste(base.url, toupper(Symbol), sep=""), n=1)
      ss <- try(strsplit(gsub("\xae|\\?", "", line1), ",")[[1]])
      if (!inherits(ss, "try-error") && length(ss) > 1) {
        attr(out, "Description") <- ss[1]
        attr(out, "Date") <- as.Date(tail(ss, 1), format="%m/%d/%Y")
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


iPathSymbols <- function() {
  readHTMLTable("http://www.ipathetn.com/us/product_information", 
                stringsAsFactors=FALSE)[[2]][, 2]
}
