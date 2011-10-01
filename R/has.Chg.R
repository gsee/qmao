#' Check for Bid, Ask, Mid, and/or Trade columns in data
#' 
#' A set of functions to check for appropriate BAM and BATM column names within
#' a data object, as well as the availability and positions of those columns.
#' 
#' \code{is.BAM} (and \code{is.BATM}, similarly) will only return TRUE if there
#' are columns for Bid, Ask, Mid (and Trade for \code{is.BATM}).  Additional
#' columns will not affect the value.
#' 
#' @aliases has.AskSize has.BidSize has.Chg has.BAM has.BATM is.BAM is.BATM has.Mid
#' @param x data object
#' @param which display position of match
#' @return A logical value indicating success or failure by default.
#' 
#' If which=TRUE, a numeric value representing the column position will be
#' returned.
#' 
#' \code{is.BAM} and \code{is.BATM} return a single value of TRUE or FALSE.
#' @author Garrett See (but based on -- and mostly copied from -- Jeffrey A.
#' Ryan's quantmod)
#' @keywords utilities
#' @export
#' @rdname has.Chg
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

#' @export
#' @rdname has.Chg
has.AskSize <-
function(x, which = FALSE)
{
   colAttr <- attr(x, "AskSize")
   if(!is.null(colAttr))
     return(if(which) colAttr else TRUE)

   loc <- grep("(ask|offer).*(size|qty|quantity)", colnames(x), ignore.case=TRUE)
   if (!identical(loc, integer(0))) {
       return(if(which) loc else TRUE)
   } else FALSE
}

#' @export
#' @rdname has.Chg
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

#' @export
#' @rdname has.Chg
is.BAM <-
function(x) {
	if (all(has.Bid(x), has.Ask(x), has.Mid(x))) {
        TRUE
    }
    else FALSE
}

#' @export
#' @rdname has.Chg
has.BAM <-
function (x, which = FALSE)
{
    if (which) {
        c(has.Bid(x, 1), has.Ask(x,1), has.Mid(x,1))
    } else {
        c(has.Bid(x), has.Ask(x), has.Mid(x))
    }
}

#' @export
#' @rdname has.Chg
has.BATM <-
function (x, which = FALSE)
{
    if (which) {
        c(has.Bid(x, 1), has.Ask(x,1), has.Trade(x,1), has.Mid(x,1))
    } else {
        c(has.Bid(x), has.Ask(x), has.Trade(x), has.Mid(x))
    }
}

#' @export
#' @rdname has.Chg
is.BATM <-
function(x) {
	if (all(has.Bid(x), has.Ask(x), has.Trade(x), has.Mid(x))) {
        TRUE
    }
    else FALSE
}

#' @export
#' @rdname has.Chg
has.Mid <- function(x, which=FALSE) {
    colAttr <- attr(x, "Mid")
    if(!is.null(colAttr))
        return(if(which) colAttr else TRUE)

	loc <- grep("Mid", colnames(x), ignore.case = TRUE)
    if (!identical(loc, integer(0))) 
        return(ifelse(which, loc, TRUE))
    ifelse(which, loc, FALSE)
}

