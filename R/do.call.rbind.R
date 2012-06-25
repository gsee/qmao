#' Faster way to \code{do.call(rbind, lst)}
#' 
#' This is a functionalized version of Dominik's answer here: 
#' \url{http://stackoverflow.com/questions/7224938/can-i-rbind-be-parallelized-in-r}
#' it does what do.call(rbind, lst) would do, but faster and with less memory usage
#'
#' @param lst a list to be rbound. (No guaranteed this funtion will offer an improvement unless
#' \code{lst} is a lits of xts objects)
#' @references \url{http://stackoverflow.com/questions/7224938/can-i-rbind-be-parallelized-in-r}
#' @examples
#' x <- xts(rnorm(10000), Sys.time()-10000:1*60)
#' splx <- split(x, 'days')
#' dcrx <- do.call.rbind(splx)
#' identical(x, dcrx)
#' @export
do.call.rbind <- function(lst) {
    while(length(lst) > 1) {
        idxlst <- seq(from=1, to=length(lst), by=2)

        lst <- lapply(idxlst, function(i) {
            if(i==length(lst)) { return(lst[[i]]) }

            return(rbind(lst[[i]], lst[[i+1]]))
        })
    }
    lst[[1]]
}


