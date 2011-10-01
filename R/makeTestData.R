#' Split data to create in-sample and out-of-sample data sets
#' 
#' #Give it an xts object and hold out pct, and it gives list with #first
#' (1-\code{ho.pct}) percent of data, and last \code{ho.pct} of data
#' 
#' 
#' @param x xts data object
#' @param ho.pct percent of data to use for out-of-sample
#' @return \item{inSample}{first data set; used to create model}
#' \item{outOfSample}{second data set for confirming results found using 1st
#' dataset}
#' @author Garrett See
#' @examples
#' \dontrun{
#' getSymbols('SPY')
#' splitdata <- makeTestData(SPY)
#' tail(splitdata[[1]])
#' head(splitdata[[2]])
#' }
#' @export
makeTestData <-
function(x, ho.pct=0.20) 
{
#Give it an xts object and hold out pct, and it gives list with 
#first (1-ho.pct) percent of data, and last ho.pct of data
	ins.from <- index(first(x))
	ins.to <- index( x[floor(length(x[,1])*(1-ho.pct))]   )
	mid.idx <- min((floor(length(x[,1])*(1-ho.pct))+1),length(x[,1]))	
	ous.from <- index(x[mid.idx,])
	ous.to <- index(last(x))
	out <- list(inSample = x[paste(ins.from,ins.to,sep="::")], 
				outOfSample = x[paste(ous.from,ous.to,sep="::")])
	out
}

