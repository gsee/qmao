has.Chg <-
function(x, which=FALSE) {
    colAttr <- attr(x, "Chg")
    if(!is.null(colAttr))
        return(if(which) colAttr else TRUE)    
	loc <- grep("(chg|change)", colnames(x), ignore.case=TRUE)
    if (!identical(loc, integer(0))) 
        return(ifelse(which, loc, TRUE))
    ifelse(which, loc, FALSE)
}

