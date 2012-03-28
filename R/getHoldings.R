#download.file('https://www.spdrs.com/library-content/public/public-files/etfnav.csv?docname=Most%20Recent%20Net%20Asset%20Values&onyx_code1=1299&onyx_code2=NA', destfile=tmp)

#There is a link called "Most Recent Net Asset Values" on https://www.spdrs.com/product/
#it has all the SPDR funds, and it should be used here instead of just the sectorspdrs.



#' get the holdings of an ETF
#' 
#' get the holdings of an iShares or Sector SPDR ETF.
#' 
#' Only iShares and the 8 Sector SPDR ETFs are supported.
#' 
#' Some iShares ETFs have more than one holding listed with the same Symbol.
#' In the case that Symbols are duplicated, a \dQuote{duplicates} attribute
#' will be added to the returned object.  Also, since rownames cannot be 
#' duplicated, they will be made unique with \code{\link{make.unique}}
#' 
#' @param Symbols chr Symbols of ETFs
#' @param env environment to store the holdings data in
#' @param auto.assign assign data?
#' @return An object classed as \dQuote{weights} will be created that is a 
#' data.frame with columns for holdings' weights and names.  If called with 
#' \code{auto.assign=TRUE}, it will be assigned in \code{env} with names that 
#' are \code{Symbols} appended with \dQuote{.h}.  Otherwise, the 
#' \dQuote{weights} will be returned and not stored.
#' @note This should get SPDR data from a csv that can be downloaded at
#' https://www.spdrs.com/product. Could also be updated to return more info
#' than just weights and Symbols.
#' @author Garrett See
#' @references Sector SPDRs \url{www.sectorspdr.com} iShares
#' \url{www.ishares.com}
#' @examples
#' 
#' \dontrun{
#' getHoldings('XLF')
#' getHoldings('IVE')
#' XLF.h
#' IVE.h
#' }
#' @export
getHoldings <-function(Symbols, env=.GlobalEnv, auto.assign=TRUE) {
	spdr.syms <- c('XLY','XLP','XLE','XLF','XLV','XLI','XLB','XLK','XLU')
	if (missing(Symbols)) Symbols <- c(ishr.syms,spdr.syms)
	ishr.out <- spdr.out <- NULL
    if (length(Symbols) > 1 && !auto.assign) stop('auto.assign must be TRUE for more than 1 symbol.')
	for (symbol in Symbols) {
		if(!is.na(match(symbol,spdr.syms))) {
			tmp <- tempfile()
			download.file(paste("http://www.sectorspdr.com/content/?do=indexComposition&symbol=", 
								symbol, "&filetype=csv", sep=""), destfile=tmp) 
			fr <- read.csv(tmp,sep="\t",stringsAsFactors=FALSE)
			unlink(tmp)
			fr <- data.frame(fr[, c(4, 2)], row.names=as.character(fr[,3]),
                             stringsAsFactors=FALSE)			
			colnames(fr) <- c(paste(symbol,'Weight',sep='.'), "Name")
            class(fr) <- c("weights", "data.frame")
			#fr2 <- data.frame(as.character(fr[,3]),as.character(fr[,2]),as.numeric(fr[,4]))
			#colnames(fr) <- c('Symbol','Name','Weight')
			if (auto.assign) assign(paste(symbol,'h',sep='.'),fr,pos=env)
			spdr.out <- c(spdr.out,paste(symbol,'h',sep='.'))
		} else {
            tmp <- tempfile()
	        download.file("http://us.ishares.com/product_info/fund/excel_profile.htm",destfile=tmp)
	        ishr.syms <- read.csv(tmp)
	        ishr.syms <- as.character(ishr.syms$Fund.Name)[-1]
	        unlink(tmp)
            if(symbol %in% ishr.syms) {
			    tmp <- tempfile()
			    download.file(paste('http://us.ishares.com/product_info/fund/excel_holdings.htm?ticker=',symbol,sep=""), destfile=tmp)
			    fr <- read.csv(tmp, skip=11, stringsAsFactors=FALSE)
			    unlink(tmp)
			    fr <- fr[1:(length(fr[,1])-3),c(1:3)]
			    fr <- fr[fr$Symbol!='--',] #maybe this is dangerous, but I'm ignoring stuff with Symbol=="--" (e.g. BLACKROCK FDS III)
			    #colnames(fr) <- c('Symbol','Name',paste(symbol,'Weight',sep='.'))
			    fr[, 1] <- make.names(gsub(" ", "", fr[, 1]))
                rowsyms <- fr[, 1]
                dupes <- character(0)
                if (any(duplicated(rowsyms))) {
                    dupes <- rowsyms[duplicated(rowsyms)]
                    warning(paste("Some Symbols duplicated:", paste(dupes, collapse=" ")))
                }
                fr <- data.frame(fr[, 3:2], row.names=make.unique(rowsyms))
			    colnames(fr) <- c(paste(symbol,'Weight',sep='.'), "Name")
                if (length(dupes) > 0) attr(fr, "duplicates") <- dupes
                class(fr) <- c("weights", "data.frame")
                if (auto.assign) assign(paste(symbol,'h',sep='.'),fr,pos=env)
			    ishr.out <- c(ishr.out,paste(symbol,'h',sep='.'))
            } else stop("Unrecognized ETF. Make sure your Symbols are either iShares or Sector SPDRs.")
		} 	
	}
    if (auto.assign) {
    	paste(c(ishr.out,spdr.out))
    } else fr
}

