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

