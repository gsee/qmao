#' Load Data from the CBOE Futures Exchange website
#' 
#' Download \code{Symbols} to specified \code{env} from \sQuote{cfe.cboe.com}. This method is 
#' not to be called directly; instead, a call to \code{getSymbols(symbols, src='cfe')} will in
#' turn call this method. This is a wrapper to get end-of-day historical data for the futures 
#' that are, or have been, listed on the CBOE Futures Exchange.
#'
#' \code{Symbols} can be a vector of symbols formatted like VX_U11, or GV_M10.  Alternatively, the
#' symbol roots can be used (e.g. 'VX', 'GV') and values can be provided for Month and Year.
#' If roots are provided, but \code{Year} and \code{Month} is \code{NULL}, 
#' the current year as reported by \code{Sys.Date()} will be used, and either the current month
#' or, for "VT" (which is the only quarterly contract), the month of the end of the last quarter.
#' See examples. 
#'
#' The raw data will contain zero values for the first few rows. Also, the last row will have
#' zeros in every column except the \sQuote{Settle} column. By default, these rows, along with
#' any other rows that have a zero Close price will be removed.  If you would like to keep these rows
#' (e.g. if you need the final settlement value) use \code{nonzero.close=FALSE}. 
#' 
#' On March 26, 2007, The prices of VX and DV futures contracts were rescaled (divided by 10)
#' \dQuote{to bring the traded futures contract prices in line with the underlying index values...}
#' If \code{rescale=TRUE} (Default) prices for these instruments from prior to March 26, 2007 will
#' be divided by 10 ("rescaled").  See References.
#' @param Symbols vector of names of instruments formated like e.g. "VX_U09", "GV_Z10", etc. OR 
#' if \code{Month} and \code{Year} are provided a vector of names of root symbols, eg. "VX","GV",etc.
#' @param Month optional vector of months. Can be numerical or the names of the months in English. If any 
#' element of \code{Symbols} contains an underscore, this will be ignored.
#' @param Year optional vector of years. Can either be 2 or 4 digits each. If any element of \code{Symbols} 
#' contains an underscore, this will be ignored.
#' @param from retrieve data no earlier than this data (2004-06-01)
#' @param to retrieve data through this data (Sys.Date())
#' @param nonzero.close if \code{TRUE} rows where \sQuote{Close} is zero will be removed.
#' @param rescale Should data from before March 26, 2007 be adjusted? See Details and References. Only applicable if \code{Symbols} is "VX" or "DV".
#' @param env where to create objects (.GlobalEnv)
#' @param return.class class of returned object
#' @param index.class class of returned object index (xts only)
#' @param \dots additional arguments
#' @return will load data into the specified environment -- one object for each file downloaded.
#' @author Garrett See, based on Jeff Ryan's quantmod framework
#' @references \url{http://cfe.cboe.com/Data/HistoricalData.aspx#VT}
#'
#' \url{http://cfe.cboe.com/framed/PDFframed.aspx?content=/publish/CFEinfocirc/CFEIC07-003\%20.pdf&sectionName=SEC_ABOUT_CFE&title=CBOE\%20-\%20CFEIC07-003\%20Rescaling\%20of\%20VIX\%20and\%20VXD\%20Futures\%20Contracts}
#' @seealso \code{\link{remove_zero_rows}} for removing rows where a column has zero values.  
#'
#' \code{getSymbols}, \code{setSymbolLookup}
#' @TODO Add suffix.format arg for making symbols
#' Add support for reading suffix_ids with 1 digit years, and/or 3 letter month codes.
#' @note Currently listed contracts:
#' VIX Futures (VX), Mini-VIX Futures (VM),
#' CBOE S&P 500 3-Month Variance Futures (VT),
#' CBOE Gold ETF Volatility Index Futures (GV)
#'
#' Delisted contracts: "DV","BX","VN","VR","VA"
#' @examples
#' \dontrun{
#' getSymbols(c("VX_U11", "VX_V11"),src='cfe')
#' #all contracts expiring in 2010 and 2011.
#' getSymbols("VX",Month=1:12,Year=2010:2011,src='cfe') 
#' #getSymbols("VX",Month=1:12,Year=10:11,src='cfe') #same
#' #The contracts expiring this month:
#' getSymbols(c("VM","GV"),src='cfe')
#'
#' setSymbolLookup(VX='cfe') #so we don't have to specify src anymore
#' getSymbols("VX",Month=1:3,Year=2005)
#' }
#' @export
getSymbols.cfe <- function(Symbols, 
                            Month=NULL, 
                            Year=NULL, 
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
            if (is.null(Month)) {
                if (!is.null(xargs) && hasArg(Months)) {
                    Month <- xargs[["Months"]]
                } else { 
                    if(any(Roots == "VT")) { 
                        round(as.numeric(format(Sys.Date(), "%m"))/3)*3
                    } else as.numeric(format(Sys.Date(), "%m"))
                } 
            }
            if (is.null(Year)) Year <- ifelse(hasArg(Years), Years, format(Sys.Date(),"%Y"))
            Year[nchar(Year) == 4] <- substr(Year[nchar(Year) == 4], 3, 4)
            Year <- sprintf("%02d",as.numeric(Year))
            if (is.numeric(Month)) Month <- C2M()[Month]
            Symbols <- c(Symbols, paste(Root, 
                        as.vector(t(sapply(Month,
                        FUN=function(x) paste(M2C(x), Year, sep="")))), 
                        sep="_"))
            #sym.file <- paste(M2C(Month), Year, "_", Symbol, ".csv",sep="")
            
        }    
    } 

    #TODO: If length of suffix_id != 3, use some magic to support formats VX_U1, VX_SEP1, VX_SEP11
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
        fr <- read.csv(tmp)
        unlink(tmp)
        if(verbose) cat("done.\n")
        fr <- fr[,-2]
        fr <- xts(as.matrix(fr[, -1]), as.POSIXct(fr[, 1], format="%m/%d/%Y", tz = Sys.getenv("TZ")), 
                src = "cfe", updated = Sys.time())
        colnames(fr) <- paste(toupper(Symbols[[i]]), 
            c("Open", "High", "Low", "Close", "Settle", "Change", "Volume", "EFP", "OpInt"), 
            sep = ".")
        fr <- fr[paste(from,to,sep="/")] #Subset by from and to
        if (nonzero.close && !identical(integer(0), grep('Close', colnames(fr), ignore.case = TRUE)) ) {
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


