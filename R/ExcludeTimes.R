#' Exclude a \code{timespan} from an xts object
#' @param x xts object
#' @param timespan xts-style time-of-day subset
#' @return xts object that does not include data during the specified 
#'   \code{timespan}
#' @author Garrett See
#' @seealso \code{\link{TimeOfDaySubset}}
#' @examples
#' .seq <- seq_len(864000)
#' a <- xts(.seq, Sys.time() + .seq)
#' b <-  ExcludeTimes(a, "T01:00/T23:00")
#' diff(b)[diff(b) != 1]
#' 
#' b <-  ExcludeTimes(a, "T02:00/T01:00")
#' diff(b)[diff(b) != 1]
#' @export
ExcludeTimes <- function(x, timespan) {
  if (substr(timespan, 1, 1) != "T" || !all(isTRUE(grepl("/T", timespan)))) {
    stop('timespan should be formatted like "T15:00:00/T16:00:00"')
  }
  ts <- gsub("T", "", timespan)
  pts <- strsplit(ts, "/")[[1]]
  pday <- .parseISO8601("T00:00/T23:59:59.999")
  ptspan <- .parseISO8601(timespan) #parsed timespan
  dbeg <- format(pday[["first.time"]], "%H:%M:%OS6")    #day begining
  dend <- format(pday[["last.time"]], "%H:%M:%OS6")     #day ending
  sbeg <- format(ptspan[["first.time"]], "%H:%M:%OS6")  #session beginning
  send <- pts[2]
  if (ptspan[["first.time"]] + 60 * 60 * 24 * 364 < ptspan[["last.time"]]) {
    #e.g. "T08:30/T15:00"
    sess1 <- paste("T", dbeg, "/T", format(ptspan[["first.time"]] - 0.0000001, "%H:%M:%OS6"), sep="")
    sess2 <- paste("T", send, "/T", dend, sep="")
  } else {
    #timespan <- "T18:00/T03:00" 
    # should be left with "T03:00/T18:00" # And maybe should do that explicitly
    sess1 <- paste("T", dbeg, "/T", send, sep="")  # 00:00:00/03:00:00
    sess2 <- paste("T", sbeg, "/T", dend, sep="")  # 18:00:00/23:59:99
  }
  rbind(x[sess1], x[sess2])
}
