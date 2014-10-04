#' Load Data from the CBOE Futures Exchange website...
#' 
#' Load Data from the CBOE Futures Exchange website
#' 
#' Download \code{Symbols} to specified \code{env} from \sQuote{cfe.cboe.com}.
#' This method is not to be called directly; instead, a call to
#' \code{getSymbols(symbols, src='cfe')} will in turn call this method. This is
#' a wrapper to get end-of-day historical data for the futures that are, or
#' have been, listed on the CBOE Futures Exchange.
#' 
#' \code{Symbols} can be a vector of symbols formatted like VX_U11, or GV_M10.
#' Alternatively, the symbol roots can be used (e.g. 'VX', 'GV') and values can
#' be provided for \code{Months} and \code{Years}. If roots are provided, but 
#' \code{Years} and \code{Months} is \code{NULL}, the current year as reported by
#' \code{Sys.Date()} will be used, and either the current month, or, for "VT" 
#' and "VA" (which are quarterly contracts), the month of the end of the last
#' quarter, or, for "RPXC", (which is biannual), the most recent March or Sep
#' contract. See examples.
#' 
#' The raw data will contain zero values for the first few rows. Also, the last
#' row will have zeros in every column except the \sQuote{Settle} column. By
#' default, these rows, along with any other rows that have a zero Close price
#' will be removed.  If you would like to keep these rows (e.g. if you need the
#' final settlement value) use \code{nonzero.close=FALSE}.
#' 
#' On March 26, 2007, The prices of VX and DV futures contracts were rescaled
#' (divided by 10) \dQuote{to bring the traded futures contract prices in line
#' with the underlying index values...} If \code{rescale=TRUE} (Default) prices
#' for these instruments from prior to March 26, 2007 will be divided by 10
#' ("rescaled").  See References.
#' 
#' @param Symbols vector of names of instruments formated like e.g. "VX_U09",
#' "GV_Z10", etc. OR if \code{Months} and \code{Years} are provided a vector of
#' names of root symbols, eg. "VX","GV",etc.
#' @param Months optional vector of months. Can be numerical or the names of the
#' months in English. If any element of \code{Symbols} contains an underscore,
#' this will be ignored.
#' @param Years optional vector of years. Can either be 2 or 4 digits each. If
#' any element of \code{Symbols} contains an underscore, this will be ignored.
#' @param from retrieve data no earlier than this data (2004-06-01)
#' @param to retrieve data through this data (Sys.Date())
#' @param nonzero.close if \code{TRUE} rows where \sQuote{Close} is zero will
#' be removed.
#' @param rescale Should data from before March 26, 2007 be adjusted? See
#' Details and References. Only applicable if \code{Symbols} is "VX" or "DV".
#' @param env where to create objects (.GlobalEnv)
#' @param return.class class of returned object
#' @param index.class class of returned object index (xts only)
#' @param \dots additional arguments
#' @return will load data into the specified environment -- one object for each
#' file downloaded.
#'
#' @note Currently listed contracts: 
#' \itemize{
#' \item{VSW -}{ CBOE Short Term Volatility Index Futures (VSW)}
#' \item{VX -}{ CBOE S&P 500 Volatility Index (VIX) Futures}
#' \item{VU -}{ CBOE Russell 2000 Volatility Index (RVX) Futures}
#' \item{VN -}{ CBOE Nasdaq-100 Volatility Index (VXN) Futures (was delisted in 2009 and relisted in July 2012)}
#' \item{VA -}{ S&P 500 Variance Futures  (product launched December 10th, 2012)}
#' \item{VXEM -}{ CBOE Emerging Markets ETF Volatility Index (VXEEM) Security Futures}
#' \item{VXEW -}{ CBOE Brazil ETF Volatility Index (VXEWZ) Security Futures}
#' \item{GV -}{ CBOE Gold ETF Volatility Index (GVZ) Security Futures}
#' \item{OV -}{ CBOE Crude Oil ETF Volatility Index (OVX) Security Futures} 
#' }
#'
#' Delisted contracts: 
#' \itemize{
#' \item{VM -}{ CBOE Mini-VIX Futures} 
#' \item{RPXC -}{ Radar Logic 28-Day Real Estate Index (RPX) Future}
#' \item{VT -}{ CBOE S&P 500 3-Month Variance Futures}
#' \item{BX -}{ CBOE S&P 500 BuyWrite Index Futures (BX)} 
#' \item{VR -}{ RUSSELL 2000 Volatility Index Futures (VR)}
#' \item{VA -}{ CBOE S&P 500 12-Month Variance Futures (VA) (product delisted March 18, 2011)}
#' \item{DV -}{ CBOE DJIA Volatility Index (DV)}
#' }
#' 
#' @author Garrett See, based on Jeff Ryan's quantmod framework
#' @seealso \code{\link{remove_zero_rows}} for removing rows where a column has zero values.
#' \code{getSymbols}, \code{setSymbolLookup}
#' @references 
#' \url{http://cfe.cboe.com/Data/HistoricalData.aspx#VT}, 
#'
#' \url{http://tinyurl.com/CFE-VIX-VXN-Rescaling}
#' @examples
#' \dontrun{
#' getSymbols(c("VX_U11", "VX_V11"),src='cfe')
#' #all contracts expiring in 2010 and 2011.
#' getSymbols("VX",Months=1:12,Years=2010:2011,src='cfe') 
#' #getSymbols("VX",Months=1:12,Years=10:11,src='cfe') #same
#' #The contracts expiring this month:
#' getSymbols(c("VM","GV"),src='cfe')
#' 
#' setSymbolLookup(VX='cfe') #so we don't have to specify src anymore
#' getSymbols("VX",Months=1:3,Years=2005)
#' }
#' @export
getSymbols.cfe <- function(Symbols, 
                            Months=NULL, 
                            Years=NULL, 
                            from='2004-06-01',
                            to=Sys.Date(),
                            nonzero.close=TRUE,
                            rescale=TRUE,
                            env, 
                            return.class='xts', 
                            index.class='Date', ...) {
    importDefaults("getSymbols.cfe")
    this.env <- environment()
    for (var in names(list(...))) {
        assign(var, list(...)[[var]], this.env)
    }
    default.return.class <- return.class
    if (missing(verbose)) 
        verbose <- FALSE
    if (missing(auto.assign)) 
        auto.assign <- TRUE
    if (all(Symbols == gsub("_","",Symbols)) ){
        Roots <- Symbols
        Symbols <- NULL        

        xargs <-list(...) 
        if(length(xargs)==0) xargs=NULL
        
        for (Root in Roots) {
            #TODO: add option to get rolling contract if month and year aren't specfied?
            if (is.null(Months)) {
                Months <- if(Root %in% c("VT", "VA")) { 
                        floor(as.numeric(format(Sys.Date(), "%m"))/3)*3
                    } else if (Root == "RPXC") {
                        floor(as.numeric(format(Sys.Date(), "%m"))/6)*6 + 3
                    } else as.numeric(format(Sys.Date(), "%m"))
            }
            if (is.null(Years)) Years <- format(Sys.Date(),"%Y")
            Years[nchar(Years) == 4] <- substr(Years[nchar(Years) == 4], 3, 4)
            Years <- sprintf("%02d",as.numeric(Years))
            if (is.numeric(Months)) Months <- C2M()[Months]
            Symbols <- c(Symbols, 
                         paste(Root, 
                               as.character(outer(vapply(Months, M2C, 
                                                         FUN.VALUE='', 
                                                         USE.NAMES=FALSE), 
                                                  Years, paste0)), sep="_"))
        }    
    } 

    #TODO: If length of suffix_id != 3, use parse_id function to support formats VX_U1, VX_SEP1, VX_SEP11
    #TODO: format output symbols the same as input?...maybe add option to do that.

    symout <- NULL
    cfe.URL <- "http://cfe.cboe.com/Publish/ScheduledTask/MktData/datahouse/CFE_"
    for (i in 1:length(Symbols)) {
        return.class <- getSymbolLookup()[[Symbols[[i]]]]$return.class
        return.class <- ifelse(is.null(return.class), default.return.class, 
            return.class)
        if (verbose) 
            cat("loading ", Symbols[[i]], ".....")
        sym.file <- paste(strsplit(Symbols[[i]],"_")[[1]][-1], "_", 
                        strsplit(Symbols[[i]],"_")[[1]][1], ".csv", sep="")
        tmp <- tempfile()
        tmptry <- try(download.file(paste(cfe.URL,sym.file,sep=""), destfile=tmp, quiet=!verbose),TRUE)
        if(inherits(tmptry,'try-error')) next()
        #if (!grepl("Trade Date", readLines(tmp, 1))) { #
        fr <- if (grepl("Terms and Conditions", readLines(tmp, 1))) { #if T&C is in first row, skip that row
            read.csv(tmp, skip=1)
        } else read.csv(tmp)
        
        unlink(tmp)
        if(verbose) cat("done.\n")
        fr <- fr[,-2]
        fr <- xts(as.matrix(fr[, -1]), as.POSIXct(fr[, 1], format="%m/%d/%Y", tz = Sys.getenv("TZ")), 
                src = "cfe", updated = Sys.time())
        colnames(fr) <- paste(toupper(Symbols[[i]]), 
            c("Open", "High", "Low", "Close", "Settle", "Change", "Volume", "EFP", "OpInt"), 
            sep = ".")
        fr <- fr[paste(from,to,sep="/")] #Subset by from and to
        
        if (nonzero.close && !identical(integer(0), grep('Close', colnames(fr), ignore.case = TRUE)) && NROW(fr) > 0) {
            {
                volm <- fr[, grep('Close',colnames(fr),ignore.case=TRUE)]
                fr <- fr[volm > 0]
            }            
        }
        if (rescale && (strsplit(Symbols[[i]],"_")[[1]][1] == "VX" || strsplit(Symbols[[i]],"_")[[1]][1] == "DV") && length(fr["::2007-03-25",1])) {
            idx <- index(fr["::2007-03-25"])            
            fr[idx,1:6] <- fr[idx,1:6]/10
        }

        fr <- quantmod:::convert.time.series(fr = fr, return.class = return.class)
        if (is.xts(fr)) 
            indexClass(fr) <- index.class
        Symbols[[i]] <- toupper(Symbols[[i]])

        if (auto.assign && length(fr[,1]) > 0 && !any(is.na(index(fr))) ) { 
            assign(Symbols[[i]], fr, env)
            symout <- c(symout, Symbols[[i]])        
        }
        if (i >= 5 && length(Symbols) > 5) {
            message("pausing .1 seconds between requests for more than 5 symbols")
            Sys.sleep(.1)
        }
    }
    if (auto.assign) 
        return(symout)
    return(fr)
}



#' View the CBOE Expiration Calendar
#' 
#' Download and view the CBOE Expiration Calendar for a given year in pdf
#' format, or view the pdf on the web without downloading it.
#' 
#' 
#' @aliases CBOEcalendar CFEcalendar
#' @param year 4 digit year of the calendar that you would like to view.
#' Defaults to the current year as determined by \code{Sys.Date()}
#' @param show what to show. Either \dQuote{pdf} or \dQuote{webpage}.
#' (Alternatively, can be be numeric: 1 for \dQuote{pdf}, 2 for
#' \dQuote{webpage})
#' @return called for side-effect
#' @author Garrett See
#' @examples
#' 
#' \dontrun{
#' CBOEcalendar() #This year's calendar in your pdf viewer.
#' CBOEcalendar(2010, show='web') #open webpage with 2010 calendar 
#' }
#' @export
CBOEcalendar <- function(year=format(Sys.Date(),'%Y'), show=c("pdf", "webpage")) {
    #url <- if (year < 2014) {
    #    paste("http://www.cboe.com/AboutCBOE/xcal", year, ".pdf", sep="")
    #} else {
    #    paste("http://www.cboe.com/TradTool/", year, "_CBOEwebsite.pdf", sep="")
    #}
    url <- paste("http://www.cboe.com/AboutCBOE/xcal", year, ".pdf", sep="")
    if (is.numeric(show)) show <- c("pdf","webpage")[show]
    switch (show[[1]], 
        pdf={
            tmp <- tempfile()
            download.file(url, destfile=tmp)
            if (.Platform$OS.type == "windows") { #thanks to Jeff Ryan's "IBrokersRef" function for this if else usage
                shell.exec(tmp) 
            } else system(paste(shQuote(getOption("pdfviewer")), shQuote(tmp)), wait=FALSE)
        }, webpage=, web=, url= {
            browseURL(url) 
        })
}

#' @export
#' @rdname CBOEcalendar
CFEcalendar <- CBOEcalendar
