applyFilter <-
function(BAM,filter=0.0001) {
    if (has.Ad(BAM)) tmp <- adjustBAM(BAM)[,1:2]
    else tmp <- BAM[,1:2]
    #TODO: use Bid, and Ask functions instead of requiring them to 
    #be in columns 1 and 2.	
    tmp.bas <- tmp[,2]-tmp[,1]
	tmp.q <- as.numeric(quantile(tmp.bas,prob=(1-filter-0.00000000001)))
	tmp <- tmp[tmp[,2]-tmp[,1] <= tmp.q]
	BAM[index(tmp)]
}

