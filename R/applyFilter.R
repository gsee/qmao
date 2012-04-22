#' Filter out bad prices
#' 
#' Deletes data where the Bid-Ask spread is extremely wide, which is probably a
#' bad print
#' 
#' If you want to delete rows where the bid ask spread is wider than it is 98
#' pct of the time, use filter=0.02
#' 
#' @param BAM An xts object that has Bid, Ask, and
#' @param filter a very small number that is the percentage of data to delete
#' @param verbose TRUE/FALSE. If TRUE, the number of rows that were removed will
#'   be printed to the standard output connection via \code{\link{cat}}
#' @return Same as BAM only with fewer rows
#' @note If applyFilter is called with a filter value of 0, it may still filter
#' the row with the widest Bid-Ask spread.
#' 
#' This is intended to clean data with extremely low bid prices, or extremely
#' high ask prices, but currently does not filter out rows with high bid prices
#' or low ask prices.
#' @author Garrett See
#' @seealso quantile
#' @examples
#' 
#' \dontrun{
#' #Create psuedo-BAM data
#' getSymbols('SPY')
#' SPY <- cbind(Lo(SPY),Hi(SPY),Cl(SPY))
#' colnames(SPY) <- paste("SPY",c('Bid.Price','Ask.Price','Mid.Price'),sep='.')
#' #make some bad prints
#' SPY[length(SPY[,1])-4,1] <- 0
#' SPY[length(SPY[,1])-3,2] <- 10000
#' SPY[length(SPY[,1])-1,1] <- 0
#' tail(SPY) #look at the raw data
#' SPY <- applyFilter(SPY, filter=1e-02)
#' tail(SPY)
#' }
#' #@export
applyFilter <-
function(BAM, filter=0.0001, verbose=TRUE) {
    if (!has.Bid(BAM) || !has.Ask(BAM)) stop('BAM must have Bid and Ask')
    tmp <- BAM(BAM)[, 1:2]
    tmp.bas <- tmp[,2]-tmp[,1]
	tmp.q <- as.numeric(quantile(tmp.bas,prob=(1-filter-0.00000000001)))
	tmp <- tmp[tmp[,2]-tmp[,1] <= tmp.q]
    if (isTRUE(verbose)) {
        removed <- NROW(BAM) - NROW(tmp)
        cat(paste0(removed, " rows removed (", 
                   sprintf("%.2f", removed / NROW(BAM) * 100),  "%); ", 
                   NROW(tmp), " rows remain.\n"))
    }
	BAM[index(tmp)]
}

