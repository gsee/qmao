#' Make an xts object strictly regular.  
#' @param x xts object
#' @param timespan time-of-day subset string (e.g. "T08:30/T15:00"). seealso
#'   \code{\link[qmao]{TimeOfDaySubset}}
#' @param by increment of the time sequence.  seealso \code{\link{seq.POSIXt}}
#' @param tz timezone to use with \code{\link{seq.POSIXt}}
#' @param verbose logical. print to standard output the number of rows that
#'   were added to make the object strictly regular?
#' @return a strictly regular xts object
#' @author Garrett See
#' @note ALPHA code; not perfect
#' @examples
#' x <- align.time(.xts(1:1000, 60*1:1000))[-c(2, 4, 7, 8), ] # remove some rows at the begining
#' head(x[paste((start.x <- start(x)), "/")])
#' x2 <- MakeStrictlyRegular(x)
#' head(x2[paste(start.x, "/")])
#' @export
MakeStrictlyRegular <- function(x, timespan="", by="min", tz="America/Chicago",
                                verbose=TRUE) {
    stime <- format(.parseISO8601(timespan)[[1]], "%H:%M:%S")
    if (is.na(stime)) {
        stime <- "00:00:00"
    }
    etime <- format(.parseISO8601(timespan)[[2]], "%H:%M:%S")
    if (is.na(etime)) {
        etime <- "23:59:99.999"
    }
    
    beg <- as.POSIXct(paste(as.Date(start(x), tz=tz), stime), 
                      origin=as.Date("1970-01-01"))
    end <- as.POSIXct(paste(as.Date(end(x), tz=tz), etime), 
                      origin=as.Date("1970-01-01"))
    
    ## merge with empty strictly regular xts, fill forward values
    tmp <- xts(, seq.POSIXt(beg, end, by))  # zero-width, strictly regular
    ## subset by time of day, split by days, fill forward each day
    ## separately, and rbind.
    xx <- cbind(tmp, x, all=TRUE)
    if (timespan != "") {
        xx <- TimeOfDaySubset(xx, timespan)
    }
    out <- do.call.rbind(lapply(split(xx, 'days'), na.locf, na.rm=TRUE))
    ## if it has a volume column, don't fill forward the volume
    if (has.Vo(xx)) {
        vxx <- Vo(xx)
        na.idx <- index(xx)[is.na(vxx)]
        out[na.idx, grep("Volume", colnames(out), ignore.case=TRUE)] <- NA
    }
    if (isTRUE(verbose)) {
        rows <- NROW(out) - NROW(x)
        if (rows > 0) {
            cat(paste0("added ", rows, " (", 
                       sprintf("%.2f", (NROW(out) / NROW(x) - 1) * 100),  
                       "%); There are now ", NROW(out), " total rows.\n"))
        } else {
            cat(paste0("removed ", rows, " (", 
                       sprintf("%.2f", (NROW(out) / NROW(x) - 1) * 100),  
                       "%); There are now ", NROW(out), " total rows.\n"))
            
        }
    }
    colnames(out) <- colnames(x)
    out
}
