has.BidSize <-
function(x, which = FALSE)
{
   colAttr <- attr(x, "BidSize")
   if(!is.null(colAttr))
     return(if(which) colAttr else TRUE)

   loc <- grep("bid.*(size|qty|quantity)", colnames(x), ignore.case=TRUE)
   if (!identical(loc, integer(0))) {
       return(if(which) loc else TRUE)
   } else FALSE
}

