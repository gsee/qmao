#' Read masterDATA csv
#'
#' read the Name and ticker Symbol of most ETFs from
#' \href{http://www.masterdata.com/helpfiles/ETF_List_Downloads/AllTypes.csv}{this csv} 
#' at \href{http://www.masterdata.com/HelpFiles/ETF_List.htm}{masterdata.com}.
#'
#' This is used by \code{\link{getHoldings}} to determine how to find the 
#' holdings of an ETF.  It is also used by \code{\link{getHoldings.vaneck}} to
#' create a list of Van Eck (Market Vectors) Symbols to use.
#' @return a \code{data.frame} with a column for \sQuote{Name} and \code{Symbol}
#' @author Garrett See
#' @examples
#' \dontrun{
#' (etfs <- read.masterDATA())
#' }
read.masterDATA <- function() {
    etfs <- read.csv(paste0("http://www.masterdata.com/helpfiles/",
                            "ETF_List_Downloads/AllTypes.csv"), 
                     stringsAsFactors = FALSE)[, 1:2]
    #etfs[, 1] <- gsub("\xae", "", etfs[, 1])
    etfs[, 1] <- gsub("[^A-Za-z0-9 -]", "", etfs[, 1])
    etfs
}

#' Get the holdings of an ETF
#' 
#' Get the names and weights of ETF holdings.
#' 
#' This function acts like a generic function.  There are other functions, such
#' as \code{\link{getHoldings.iShares}} and \code{\link{getHoldings.selectSPDR}}
#' that act like \dQuote{methods}.  
#'
#' \code{getHoldings} looks up the \code{Symbols} in the CSV at 
#' \url{http://www.masterdata.com/helpfiles/ETF_List_Downloads/AllTypes.csv}
#' from \url{http://www.masterdata.com/HelpFiles/ETF_List.htm}.
#' It uses the \code{Name} field to determine which 
#' \code{getHoldings.*} function to call. 
#' 
#' Different \code{getHoldings.*} \dQuote{methods} will return different 
#' columns, but all of them should have the Ticker Symbol or some other 
#' identifier as the \code{rownames}, the first column should be \code{Weights}
#' (0-100), and the second column should be \code{Name} (a description of the 
#' security).  Also, the returned holdings data will be a \code{data.frame} with
#' class \code{weights}, or a \code{list} of \code{weights} classed objects.
#' 
#' Some iShares ETFs have more than one holding listed with the same Symbol.
#' In the case that Symbols are duplicated, a \dQuote{duplicates} attribute
#' will be added to the returned object.  Also, since rownames cannot be 
#' duplicated, they will be made unique with \code{\link{make.unique}}
#' 
#' @param Symbols chr Symbols of ETFs.  Presently, if not supplied the Symbols 
#'   of the nine Sector SPDRs will be used.  However, in the future it may be
#'   changed to require that \code{Symbols} be provided by the user.
#' @param env environment to store the holdings data in
#' @param auto.assign assign data?
#' @return An object classed as \dQuote{weights} will be created that is a 
#' data.frame with columns for holdings' weights and names.  If called with 
#' \code{auto.assign=TRUE}, it will be assigned in \code{env} with names that 
#' are \code{Symbols} appended with \dQuote{.h}.  Otherwise, the 
#' \dQuote{weights} will be returned and not stored.  Returned objects may have
#' more columns depending on which \code{getHoldings.*} \dQuote{method} is used.
#' @author Garrett See
#' @seealso \code{\link{getHoldings.SPDR}}, 
#'   \code{\link{getHoldings.selectSPDR}},
#'   \code{\link{getHoldings.iShares}}, \code{qmao:::getHoldings.iShares.AsOf},
#'   \code{\link{getHoldings.vaneck}}, \code{\link{getHoldings.powershares}},
#'   \code{\link{getHoldings.GlobalX}}, \code{\link{getHoldings.FirstTrust}},
#'   \code{\link{getHoldings.WisdomTree}}
#' @references 
#' \url{http://www.masterdata.com/HelpFiles/ETF_List.htm}
#' @examples
#' 
#' \dontrun{
#' getHoldings('XLB', auto.assign=FALSE) #getHoldings.SPDR or .selectSPDR depending on OS
#' getHoldings('IVE') #getHoldings.iShares
#' IVE.h
#' getHoldings(c("QQQ", "GDX")) #.powershares and .vaneck (Market Vectors)
#' getHoldings(c("XLY", "IVE", "QQQ", "GDX", "OIH", "XLU", "EPI"))
#' }
#' @export
getHoldings <-function(Symbols, env=.GlobalEnv, auto.assign=TRUE) {
    ssspdrs <- c('XLY','XLP','XLE','XLF','XLV','XLI','XLB','XLK','XLU')	
    if (missing(Symbols)) Symbols <- ssspdrs
    if (length(Symbols) > 1 && !auto.assign) {
        stop('auto.assign must be TRUE for more than 1 symbol.')
    }
    etfs <- read.masterDATA()
    etfs[, 1] <- sapply(strsplit(etfs[, 1], " "), "[", 1)
    msym <- etfs[etfs$Symbol %in% Symbols, ]
    spl.m <- split(msym, msym$Name)
    fams <- names(spl.m) # (first word of) unique fund families
    spdr.out <- ishr.out <- van.out <- pow.out <- globx.out <- fstr.out <- 
        wt.out <- dirx.out <- NULL
    # for each of the getHoldings.* functions, find the symbols that that 
    # function works with and apply it to them.  Then, remove those symbols 
    # from `Symbols` so that after we've processed everything we know how to 
    # process, `Symbols` will be the names we were given but don't know what to 
    # do with.
    if ("SPDR" %in% fams) {
        s <- spl.m[[grep("SPDR", spl.m)]][, 2]
        spdr.out <- getHoldings.SPDR(s, env=env, auto.assign=auto.assign)
        Symbols <- Symbols[!Symbols %in% s]
    }
    if ("iShares" %in% fams) {
        s <- spl.m[[grep("iShares", spl.m)]][, 2]
        ishr.out <- getHoldings.iShares(s, env=env, auto.assign=auto.assign)
        Symbols <- Symbols[!Symbols %in% s]
    }
    if ("Market" %in% fams) { # Van Eck ETFs are also called Market Vectors 
        s <- spl.m[[grep("Market", spl.m)]][, 2]
        van.out <- getHoldings.vaneck(s, env=env, auto.assign=auto.assign)
        Symbols <- Symbols[!Symbols %in% s]
    }
    # These are duplicated when case is ignored "claymore" "ipath" "powershares"
    if (any(grepl("powershares", fams, ignore.case=TRUE))) {
        s <- do.call(rbind, 
                     spl.m[grep("powershares", spl.m, ignore.case=TRUE)])[, 2]
        pow.out <- getHoldings.powershares(s, env=env, auto.assign=auto.assign)
        Symbols <- Symbols[!Symbols %in% s]
    }
    #TODO: FIXME: add support for Global X and First Trust
    if ("Global" %in% fams) { 
        s <- spl.m[[grep("Global", spl.m)]][, 2]
        globx.out <- getHoldings.GlobalX(s, env=env, auto.assign=auto.assign)
        Symbols <- Symbols[!Symbols %in% s]
    }
    if ("First" %in% fams) { 
        s <- spl.m[[grep("First", spl.m)]][, 2]
        fstr.out <- getHoldings.FirstTrust(s, env=env, auto.assign=auto.assign)
        Symbols <- Symbols[!Symbols %in% s]
    }
    if ("WisdomTree" %in% fams) {
        s <- spl.m[[grep("WisdomTree", spl.m)]][, 2]
        wt.out <- getHoldings.WisdomTree(s, env=env, auto.assign=auto.assign)
        Symbols <- Symbols[!Symbols %in% s]
    }
    if ("Direxion" %in% fams) {
        s <- spl.m[[grep("Direxion", spl.m)]][, 2]
        dirx.out <- getHoldings.Direxion(s, env=env, auto.assign=auto.assign)
        Symbols <- Symbols[!Symbols %in% s]
    }
    out <- list(spdr.out, ishr.out, van.out, pow.out, globx.out, fstr.out, 
                wt.out, dirx.out, Symbols)
    names(out) <- c("SPDR", "iShares", "VanEck", "PowerShares", "GlobalX", 
                    "FirstTrust", "WisdomTree", "Direxion", "NotFound")
    Filter(function(x) length(x) > 0, out)
}
