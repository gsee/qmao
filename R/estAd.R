estAd <-
function(x, symbol = NULL, prefer=NULL) {
	#get Adjusted price if available, else find alternative
    if (is.null(prefer)) {
	    if (has.Ad(x)) return(Ad(x))
	    else if (has.Cl(x)) return(Cl(x))
	    else if (has.Mid(x)) return(Mi(x))
	    else getPrice(x, prefer=prefer)
    } else getPrice(x,prefer=prefer)
}

