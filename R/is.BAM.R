is.BAM <-
function(x) {
	if (all(has.Bid(x), has.Ask(x), has.Mid(x))) {
        TRUE
    }
    else FALSE
}

has.BAM <-
function (x, which = FALSE)
{
    if (which) {
        c(has.Bid(x, 1), has.Ask(x,1), has.Mid(x,1))
    } else {
        c(has.Bid(x), has.Ask(x), has.Mid(x))
    }
}


BAM <- function(x)
{
    if (is.BAM(x))
        return(x[, has.BAM(x,1)])
    NULL
}


