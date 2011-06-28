is.BATM <-
function(x) {
	if (all(has.Bid(x), has.Ask(x), has.Trade(x), has.Mid(x))) {
        TRUE
    }
    else FALSE
}

BATM <- function(x)
{
    if (is.BATM(x))
        return(x[, has.BATM(x,1)])
    NULL
}


has.BATM <-
function (x, which = FALSE)
{
    if (which) {
        c(has.Bid(x, 1), has.Ask(x,1), has.Trade(x,1), has.Mid(x,1))
    } else {
        c(has.Bid(x), has.Ask(x), has.Trade(x), has.Mid(x))
    }
}

has.Mid <- function(x, which=FALSE) {
    colAttr <- attr(x, "Mid")
    if(!is.null(colAttr))
        return(if(which) colAttr else TRUE)

	loc <- grep("Mid", colnames(x), ignore.case = TRUE)
    if (!identical(loc, integer(0))) 
        return(ifelse(which, loc, TRUE))
    ifelse(which, loc, FALSE)
}

