# Garrett See

# @examples
# LastBusinessDateOfMonth("200609")
LastBusinessDateOfMonth <- function(YYYYMM) {
    dtch <- !'package:timeDate' %in% search() #if it's not attached we'll detach after using
    require(timeDate)
    YYYYMM <- if (missing(YYYYMM)) {
            format(Sys.Date(),"%Y%m") 
        } else substr(gsub("[ -]","",YYYYMM),1, 6)
    monthend <- as.Date(paste(format(as.Date(paste(YYYYMM,"15",sep=""), 
                "%Y%m%d") + 30,"%Y%m"),"01",sep=""),format='%Y%m%d') - 1
    if (monthend %in% as.Date(holidayNYSE(as.numeric(format(monthend, "%Y")))@Data) ||
            any(c("Saturday","Sunday") == weekdays(monthend))) {
        while (monthend %in% as.Date(holidayNYSE(as.numeric(format(monthend, "%Y")))@Data) || 
            any(c("Saturday","Sunday") == weekdays(monthend))) monthend <- monthend - 1    
    }
    if (dtch) detach(package:timeDate, unload=TRUE)
    monthend
}

# @examples
# getHoldings.iShares.AsOf("IYE", "200909")
getHoldings.iShares.AsOf <- function(Symbol, YYYYMM) {
    monthend <- LastBusinessDateOfMonth(YYYYMM)
    addy <- paste("http://us.ishares.com/product_info/fund/holdings/",
                    Symbol, ".htm?asofDt=", format(monthend,"%Y-%m-%d"),
                    "&periodCd=m", sep="")
    rHT <- readHTMLTable(addy)[[2]]
    colnames(rHT) <- gsub("[ \n]","",colnames(rHT))
    mvcol <- grep("MarketValue",colnames(rHT))
    rHT[, mvcol] <- as.numeric(gsub("[$,]","", rHT[ ,mvcol]))
    wcol <- grep("%NetAssets",colnames(rHT))    
    colnames(rHT)[wcol] <- 'Weight'
    rHT
}

#l <- lapply(paste("2010",sprintf("%02d", 1:12)), function(x) paste(getHoldings.iShares.AsOf("IYE", x)[,1]))
#us <- unique(do.call(c, l)) #unique symbols
#us

