# jsonbzx <- fromJSON(paste("http://www.batstrading.com/json", "bzx", 
#                         "book", Symbol, sep="/"))
# jsonbyx <- fromJSON(paste("http://www.batstrading.com/json", "byx", 
#                         "book", Symbol, sep="/"))

# getQuote ----

#' getQuote from BATS
#' 
#' \code{getQuote.BATS} is a \code{\link[quantmod]{getQuote}} "method" for BATS,
#' although it may be called directly.
#' 
#' This downloads data from BATS for a given stock and exchange and parses it 
#' into a list using \code{\link[RJSONIO]{fromJSON}}.  By default 
#' (\code{what="bats"}), this list is given a class of \code{bats} and returned.  
#' Currently, the other values for \code{what} just specify how to 
#' reformat/subset the data.
#' 
#' With \code{what="bbo"}, a \code{data.frame} will be returned with 
#' a row for each of the \code{Symbols} and columns "BidSize", "BidPrice", 
#' "AskPrice", "AskSize", "Last", "LastSize" (any of those columns that are all 
#' NA are omitted; e.g. when the market is closed the returned \code{data.frame} 
#' will likely only have the "TradeTime", "Last" and "LastSize" columns.
#' 
#' If \code{what="ladder"}, an object of class \code{ladder} is returned.  It 
#' will be a \code{matrix} at its core with 3 columns: BidQty, Price, AskQty.  
#' The Price column is from highest to lowest. There should only be a
#' BidQty or an AskQty at a given price, and the other will be NA (otherwise,
#' the market would be crossed).  An object of class \code{ladder} will also 
#' have the following attributes: "timestamp", "company", "volume", 
#' "last.price", and "last.qty"
#' 
#' When called with \code{what="depth"}, an object of class \code{depth} will
#' be returned which is very similar to a \code{ladder} object except it has
#' 4 columns and (usually) no NAs.  Instead of stacking the prices on top of 
#' each other, they are side-by-side.  A \code{depth} object is a \code{matrix}
#' at its core and has 4 columns: "BidQty", "BidPrice", "AskPrice", "AskQty".
#' Under usual market conditions it will have 5 rows.  It also has all the 
#' attributes that a \code{ladder} object has.
#' 
#' If \code{what="trades"}, an xts object is returned that has the "Qty" and
#' "Price" of the last 10 trades.
#' 
#' There are \code{print} methods for \code{bats}, \code{ladder}, and 
#' \code{depth} that make use of \code{pander::pandoc.table}
#' 
#' There are also \code{plot} methods for \code{bats}, \code{ladder}, and 
#' \code{depth}.  These make it incredibly easy to make a nice looking shiny 
#' app.
#' 
#' @param Symbols a vector of ticker symbols.  If length is greater than 1, a 
#'   list will be returned unless \code{what="bbo"} in which case a multi-row
#'   \code{data.frame} will be returned. 
#' @param what character. What to return.  See details
#' @param exch character. "bzx" or "byx".  ("opt" is completely untested and 
#'   most likely does not work)
#' @param x a bats object
#' @param ... pass through arguments
#' @return market data.  The type of object returned depends on the value of
#'   \code{what}.  See Details.  If \code{Symbols} is of length greater than 1 
#'   and type is not "bbo", a list will be returned.
#' @author Garrett See
#' @references \url{http://www.batstrading.com/market_data/}
#' @note "opt" exch has NOT been TESTED AT ALL
#' @seealso \code{\link[quantmod]{getQuote}}
#' @examples
#' \dontrun{ # can't run the examples because they require internet access
#' ## These should be run while the U.S. stock market is open
#' getQuote("SPY", src="BATS")
#' getQuote("SPY", src="BATS", exch="byx")
#' 
#' getQuote("SPY", src="BATS", what="bbo")
#' getQuote("SPY", src="BATS", what="ladder")
#' print(getQuote("SPY", src="BATS", what="ladder"), header=FALSE)
#' getQuote("SPY", src="BATS", what="depth")
#' getQuote("SPY", src="BATS", what="trades")
#'
#' plot(getQuote("SPY", src="BATS"))
#' plot(getQuote("SPY", src="BATS", what="ladder"))
#' plot(getQuote("SPY", src="BATS", what="depth"))
#' }
#' @export
#' @rdname getQuote.BATS
getQuote.BATS <- function(Symbols, 
                          what=c("bats", "bbo", "ladder", "depth", "trades"), #, "most.active"), 
                          exch=c("bzx", "byx", "opt")) {
  #require(RJSONIO) # added at top of script; fromJSON will be Imported from RJSONIO in NAMESPACE
  what <- tolower(what)
  exch <- tolower(exch)
  what <- match.arg(what)
  exch <- match.arg(exch)
  
  # if Symbols is more than 1 symbol, then return a multi-row data.frame if 
  # what="bbo" or a list if what is anything else
  if (length(Symbols) > 1L) {
    L <- setNames(lapply(Symbols, getQuote.BATS, what=what, exch=exch), Symbols)
    if (what == "bbo") {
      return(do.call(rbind, L))
    } else return(L)
  } else Symbol <- Symbols

  x <- fromJSON(paste("http://www.batstrading.com/json", exch, 
                      "book", Symbol, sep="/"))
  
  if (length(x$data$trades) > 0L) {
    x$LastSize <- as.numeric(x$data$trades[[1L]][2L])
    x$Last <- as.numeric(x$data$trades[[1L]][3L])
    x$TradeTime <- x$data$trades[[1L]][1L]
  } else {
    x$LastSize <- x$Last <- x$TradeTime <- NA
  }
  
  class(x) <- c("bats", class(x))
  if (length(x$data$asks) == 0L) x$data$asks <- NULL
  if (length(x$data$bids) == 0L) x$data$bids <- NULL
  switch(what, 
         "bats"={
           x
         }, "bbo"={
           data.frame(cbind(TradeTime=x$TradeTime,
                            BidSize=as.numeric(x$data$bids[[1L]][1L]),
                            BidPrice=as.numeric(x$data$bids[[1L]][2L]),
                            AskPrice=as.numeric(x$data$asks[[1L]][2L]),
                            AskSize=as.numeric(x$data$asks[[1L]][1L]),
                            Last=x$Last,
                            LastSize=x$LastSize, 
                            row.names=Symbol))
         }, "ladder"={
           ladder(x)  
         }, "depth"={
           depth(x)
         }, "trades"={
           trades(x)
         }) #, "active"=, "most.active"=, "mostactive"={
            #  stop('what="most.active" not yet supported')
            # })
}

#' @export
#' @rdname getQuote.BATS
getQuote.bats <- getQuote.BATS

# benchmarking -----

## See benchmark.getQuote.BATS.R for benchmark results of 5 different methods
## for converting trades data to xts.

# utility ----

asks <- function(x, rev=FALSE) {
  if (length(x$data$asks) > 0L) {
    if (isTRUE(rev)) {
      matrix(rev(unlist(x$data$asks)), ncol=2, byrow=TRUE)
    } else matrix(unlist(x$data$asks), ncol=2, byrow=TRUE)
  } else NULL
}

bids <- function(x, rev=FALSE) {
  if (length(x$data$bids) > 0L) {
    if (isTRUE(rev)) {
      matrix(rev(unlist(x$data$bids)), ncol=2, byrow=TRUE)
    } else matrix(unlist(x$data$bids), ncol=2, byrow=TRUE)
  } else NULL
}

# extractors ----

# @param x BATS data parsed by \code{fromJSON}
#x <- fromJSON(paste("http://www.batstrading.com/json", match.arg(exch), 
#                        "book", Symbol, sep="/"))
#' @rdname getQuote.BATS
ladder <- function(x) {
  out <- rbind(cbind(NA, asks(x, rev=TRUE)), cbind(bids(x), NA))
  if (NCOL(out) == 3L) colnames(out) <- c("BidQty", "Price", "AskQty")
  structure(out, timestamp=x$data$timestamp, company=x$data$company, 
            volume=x$data$volume, 
            last.price=x$Last,
            last.qty=x$LastSize, class="ladder")
}

#' @rdname getQuote.BATS
trades <- function(x) {
  ul <- unlist(rev(x$data$trades))
  if (length(ul) == 0L) return(NA)
  times <- as.POSIXct(paste(Sys.Date(), ul[c(TRUE, FALSE, FALSE)]))
  structure(setNames(xts(matrix(as.numeric(ul[c(FALSE, TRUE, TRUE)]), ncol=2, 
                                byrow=TRUE), times), c("Qty", "Price")),
            class=c("trades", "xts", "zoo"))
}

#' @rdname getQuote.BATS
depth <- function(x, ...) {
  if (inherits(x, "depth")) return(x) # simpler than making generic for now
  if (!inherits(x, 'bats')) stop("x is not a 'bats' object")
  b <- bids(x)
  a <- asks(x)
  if (length(b) > 0L && length(a) > 0L) {
    out <- cbind(b, a[, 2:1]) 
    colnames(out) <- c("BidQty", "BidPrice", "AskPrice", "AskQty")
    structure(out, timestamp=x$data$timestamp, company=x$data$company, 
              volume=x$data$volume, last.price=x$Last,
              last.qty=x$LastSize, class="depth")
  } else {
    structure(sapply(c("BidQty", "BidPrice", "AskPrice", "AskQty"), function(x) rep(NA, 5)), 
              timestamp=x$data$timestamp, company=x$data$company, 
              volume=x$data$volume, last.price=x$Last,
              last.qty=x$LastSize, class="depth")
  }
}
#depth(x)
#pandoc.table(depth(x), style='grid')

# mostActive <- function(exch=c("bzx", "bzx"), 
#                        tape=c("all", "a", "b", "c"),
#                        n=c("25", "10", "50", "100"), ...) {
#   #http://www.batstrading.com/most_active/25/
# }

# print methods -----

#' ladder class print method
#' 
#' @method print ladder
#' @S3method print ladder
#' @keywords internal
print.ladder <- function(x, style=c("grid", "multiline"), digits=6, header=TRUE, ...) {
  #require(pander) # This will be at top of script or will importFrom(pander, pandoc.table) in NAMESPACE 
  if (isTRUE(header)) {
    cat(" ", attr(x, "company"), "\n", 
        " Time:   ", attr(x, "timestamp"), "\n",
        " Volume: ", attr(x, "volume"), "\n", 
        " Last:", attr(x, "last.qty"), "@", attr(x, "last.price"))
  }
  pandoc.table(unname(x), style=match.arg(style), digits=6, ...)
}
#print(ladder(x), style='multiline')
#unclass(ladder(x))  

#' depth class print method
#' 
#' @method print depth
#' @S3method print depth
#' @keywords internal
print.depth <- function(x, style=c("simple", "multiline", "grid"), ...) {
  pandoc.table(x, style=match.arg(style), digits=6)
}

#' bats class print method
#' 
#' @method print bats
#' @S3method print bats
#' @keywords internal
print.bats <- function(x, ...) {
  print(ladder(x), ...)
  print(trades(x), ...)
}

# plot methods ----

#' ladder class plot method
#' 
#' @method plot ladder
#' @S3method plot ladder
#' @keywords internal
plot.ladder <- function(x, cex=1.5, font=2, ...) {
  #FIXME: rect can be vectorized which may be quicker; see plot.bats
  #TODO: add other attributes to plot: timestamp, company, volume, last.qty
  font <- rep(font, length.out=3) # if font is vector it can be different for bids, prices, asks.
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  par(mar=c(0, 0, 2, 0) + 0.1, ...)  
  # NOTE: User should set width and height of the plotting device 
  #       (maybe width=400, height=700, or 500 X 900)
  dev.hold() # I put this in because plot.default has it; don't know if it's needed
  on.exit(dev.flush()) # This also was taken from plot.default without much thought
  plot.new()
  n <- NROW(x)
  if (all(is.na(x))) {
    if (length(attr(x, "last.qty")) < 1L || length(attr(x, "last.price")) < 1L) {
      stop("no data to plot")
    }
    #warning("No bids or offers")
    text(0.5/2, 1, label="Quantity", col='black', font=font[[1L]], cex=cex)
    text(1.5/2, 1, label="Price", col='black', font=font[[1L]], cex=cex)
    rect(0, 8.5/10, 1/2, 9.5/10, col='grey')
    text(0.5/2, 9/10, label=attr(x, "last.qty"), col='black', font=font[[1L]], cex=cex)
    rect(1, 8.5/10, 1/2, 9.5/10, col='grey')
    text(1.5/2, 9/10, label=attr(x, "last.price"), col='black', font=font[[1L]], cex=cex)
    title(main=attr(x, "company")) #attr(x, "timestamp"))
    par(old.par)
    return(invisible(x))
  }
  
  for (i in seq_len(n)) {
    #rect(0, (i-1)/n, 1/3, i/n, col=ifelse(i <= 5, 'blue2', 'darkblue')) # I like this better, but...
    rect(0, (i-1)/n, 1/3, i/n, col=ifelse(i <= 5, 'turquoise3', 'turquoise4'))
    text((0.5)/3, (i-0.5)/n, label=x[n-i+1, 1], col="white", font=font[[1L]], cex=cex) 
    rect(1/3, (i-1)/n, 2/3, i/n, 
         col=ifelse(isTRUE(all.equal(round(as.numeric(attr(x, "last.price")), 2), 
                                     x[n-i+1, 2])), 'slategrey', 'grey'))
    text((1.5)/3, (i-0.5)/n, label=x[n-i+1, 2], col="black", font=font[[2L]], cex=cex) 
    #rect(2/3, (i-1)/n, 3/3, i/n, col=ifelse(i <= 5, 'darkred', 'red2')) # I like this better, but...
    rect(2/3, (i-1)/n, 3/3, i/n, col=ifelse(i <= 5, 'tomato4', 'tomato3'))
    #rect(2/3, (i-1)/n, 3/3, i/n, col=ifelse(i <= 5, 'firebrick4', 'firebrick3'))
    text((2.5)/3, (i-0.5)/n, label=x[n-i+1, 3], col="white", font=font[[3L]], cex=cex)
  }
  title(main=attr(x, "company"))
  par(old.par)
  invisible(x)
}
#plot(ladder(x))

#' depth class plot method
#' 
#' @method plot depth
#' @S3method plot depth
#' @keywords internal
plot.depth <- function(x, cex=1.5, font=2, bg=c("black", "white"), ...) {
  #TODO: add other attributes to plot: timestamp, company, volume, last.qty
  
  # if font is vector it can be different for bid qty, bid prc, askprice, ask qty.
  font <- rep(font, length.out=4)
  bg <- match.arg(bg)
  fg <- if(bg == "black") { "white" } else "black"
  
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  par(mar=c(0, 0, 2, 0) + 0.1, bg=bg, ...)  
  # NOTE: User should set width and height of the plotting device 
  #       (maybe width=400, height=700, or 500 X 900)
  dev.hold() # I put this in because plot.default has it; don't know if it's needed
  on.exit(dev.flush()) # This also was taken from plot.default without much thought
  plot.new()
  n <- NROW(x)
  if (all(is.na(x))) {
    if (length(attr(x, "last.qty")) < 1L || length(attr(x, "last.price")) < 1L) {
      stop("no data to plot")
    }
    #warning("No bids or offers")
    text(0.5/2, 1, label="Quantity", col=fg, font=font[[1L]], cex=cex)
    text(1.5/2, 1, label="Price", col=fg, font=font[[2L]], cex=cex)
    rect(0, 8.5/10, 1/2, 9.5/10, col='grey')
    text(0.5/2, 9/10, label=attr(x, "last.qty"), col='black', font=font[[1L]], cex=cex)
    rect(1, 8.5/10, 1/2, 9.5/10, col='grey')
    text(1.5/2, 9/10, label=as.numeric(attr(x, "last.price")), col='black', font=font[[2L]], cex=cex)
    title(main=list(attr(x, "company"), col=fg), sub=list(attr(x, "timestamp"), col=fg)) #attr(x, "timestamp")))
    par(old.par)
    return(invisible(x))
  }
  
  # TODO: I'd kind of like to have the prices fade from white to cyan and
  #       use heat colors for the quantities depending on the qty
  #ramp <- cm.colors(n*1.9)[seq_len(n)] # I like this, but it's a little too light
  #ramp <- heat.colors(n*1.9)[seq_len(n)]
  #ramp <- rev(rainbow(n, start=3/6, end=3.5/6)) # this is okay; a little too blue
  #ramp <- heat.colors(n*1.9)[seq_len(n)]
  ramp <- colorRampPalette(c("cyan", "white"))(5)  
  
  for (i in seq_len(n)) {
    rect(0, (i-1)/(n+1), 2.25/10, i/(n + 1), col=ramp[i])
    text(1.125/10, (i-0.5)/(n+1), label=x[n-i+1, 1], col="black", font=font[[1L]], cex=cex) 
    rect(2.25/10, (i-1)/(n+1), 4.5/10, i/(n+1), col=ramp[i])
    text(3.375/10, (i-0.5)/(n+1), label=x[n-i+1, 2], col="black", font=font[[2L]], cex=cex) 
    rect(5.5/10, (i-1)/(n+1), 7.75/10, i/(n+1), col=ramp[i])
    text(6.625/10, (i-0.5)/(n+1), label=x[n-i+1, 3], col="black", font=font[[3L]], cex=cex) 
    rect(7.75/10, (i-1)/(n+1), 10/10, i/(n+1), col=ramp[i])
    text(8.875/10, (i-0.5)/(n+1), label=x[n-i+1, 4], col="black", font=font[[4L]], cex=cex)
  }

  rect(0, 5/6, 2.25/10, 5.5/6, col='grey40')
  text(1.125/10, 5.25/6, label="Shares", col="white")
    
  rect(2.25/10, 5/6, 4.5/10, 5.5/6, col='grey40')
  text(3.375/10, 5.25/6, label="Price", col="white") 
  
  rect(5.5/10, 5/6, 7.75/10, 5.5/6, col='grey40')
  text(6.625/10, 5.25/6, label="Price", col="white")
  
  rect(7.75/10, 5/6, 10/10, 5.5/6, col='grey40')
  text(8.875/10, 5.25/6, label="Shares", col="white")
  
  title(main=attr(x, "company"), sub=attr(x, "timestamp"), col.main=fg, col.sub=fg)
  #title(main=list(attr(x, "company"), col=fg, cex=2), sub=list(attr(x, "timestamp"), col=fg))
  par(old.par)
  invisible(x)
}

#' bats class plot method
#' 
#' @method plot bats
#' @S3method plot bats
#' @keywords internal
plot.bats <- function(x, cex=1, ...) {
  # this is the template
  # http://www.batstrading.com/bzx/book/SPY/

  if (!inherits(x, "bats")) stop("bats object required for plot.bats method")
  
  plot.new()

  text(1/5, 13.5/14, label="Orders Accepted", col="#414141", family="sans")
  text(1/5, 12.75/14, label=format(x$data$orders, big.mark=","), col="#414141", family="sans")
  text(3.5/5, 13.5/14, label="Total Volume", col="#414141", family="sans")
  text(3.5/5, 12.75/14, label=format(x$data$volume, big.mark=","), col="#414141", family="sans")
       
  rect(0, 10/14, 2/5, 12/14, col="#565656") #"#595959"
  text(1/5, 11.5/14, label="TOP OF BOOK", col="white", font=2, family="sans", cex=cex)
  text(0.5/5, 10.5/14, label="SHARES", col="white", font=2, family="sans", cex=cex)
  text(1.5/5, 10.5/14, label="PRICE", col="white", font=2, family="sans", cex=cex)

  rect(2/5, 10/14, 5/5, 12/14, col="#565656") #"#595959"
  text(3.5/5, 11.5/14, label="LAST 10 TRADES", col="white", font=2, family="sans", cex=cex)
  text(2.5/5, 10.5/14, label="TIME", col="white", font=2, family="sans", cex=cex)
  text(3.5/5, 10.5/14, label="PRICE", col="white", font=2, family="sans", cex=cex)
  text(4.5/5, 10.5/14, label="SHARES", col="white", font=2, family="sans", cex=cex)

  # plot trades
  # First draw grid
  n <- length(x$data$trades)
  cvec <- rep(c("#EFEFEF", "#FFFFFF"), length.out=n) #color vector: grey, white, grey, etc
  rect(2/5, (10-1:10)/14, 5/5, (11-1:10)/14, col=cvec, border='lightgrey')
  
  bidramp <- colorRampPalette(c("#9bbc5b", "#c2dd96"))(5) # bids
  #bidramp <- colorRampPalette(c("#c2dd96", "#9bbc5b"))(5) # bids
  rect(0/5, (10-6:10)/14, 2/5, (11-6:10)/14, col=bidramp, border=NA)
  #askramp <- colorRampPalette(c("#99bbe8", "#cbddf3"))(5) # asks
  askramp <- colorRampPalette(c("#cbddf3", "#99bbe8"))(5) # asks # this looks opposite of BATS website
  rect(0/5, (10-1:5)/14, 2/5, (11-1:5)/14, col=askramp, border=NA)
  # draw borders
  rect(2/5, 0, 5/5, 10/14)
  rect(0/5, 5/14, 2/5, 10/14)
  rect(0/5, 0, 2/5, 5/14)
  
  bid <- bids(x)
  ask <- asks(x, rev=TRUE)[, 2:1]
  if (all(is.null(bid))) bid <- matrix(NA, 5, 2)
  if (all(is.null(ask))) ask <- matrix(NA, 5, 2)
  tob <- rbind(ask, bid)  
  
  # can I vectorize text()?
  for (i in seq_len(n)) {
    # top of book; tob
    text(0.5/5, (10.5 - i)/14, label=tob[i, 1], col='black')
    text(1.5/5, (10.5 - i)/14, label=tob[i, 2], col='black', font=ifelse(i %in% 5:6, 2, 1))
    
    # trades
    dat <- x$data$trades[[i]]
    text(2.5/5, (10.5 - i)/14, label=dat[1L], col='black')
    text(3.5/5, (10.5 - i)/14, label=dat[3L], col='black')
    text(4.5/5, (10.5 - i)/14, label=dat[2L], col='black')
  }
  title(main=x$data$company, sub=x$data$timestamp)
  invisible(x)
}

# static data for testing ----

# #dput(x)
# x <- structure(list(reload = 7539.69950675964, data = structure(list(
#     volume = 17693374, tick_type = "", asks = list(c(400, 141.94
#     ), c(24000, 141.95), c(36100, 141.96), c(41800, 141.97), 
#         c(54100, 141.98)), auction = FALSE, timestamp = "15:50:39", 
#     symbol = "SPY", trades = list(c("15:50:29", "400", "141.9400"
#     ), c("15:50:29", "200", "141.9400"), c("15:50:29", "4700", 
#     "141.9400"), c("15:50:29", "1000", "141.9400"), c("15:50:29", 
#     "500", "141.9400"), c("15:50:29", "700", "141.9400"), c("15:50:29", 
#     "700", "141.9400"), c("15:50:29", "700", "141.9400"), c("15:50:29", 
#     "700", "141.9400"), c("15:50:29", "300", "141.9400")), bids = list(
#         c(43200, 141.93), c(26540, 141.92), c(44200, 141.91), 
#         c(41500, 141.9), c(36800, 141.89)), company = "SPDR S&P 500 ETF TR TR UNIT", 
#     orders = 947488), .Names = c("volume", "tick_type", "asks", 
# "auction", "timestamp", "symbol", "trades", "bids", "company", 
# "orders")), success = TRUE), .Names = c("reload", "data", "success"
# ), class = "bats")
# 
# #dput(jsonbzx)
# jsonbzx <- structure(list(reload = 7539.69950675964, data = structure(list(
#     volume = 17693374, tick_type = "", asks = list(c(400, 141.94
#     ), c(24000, 141.95), c(36100, 141.96), c(41800, 141.97), 
#         c(54100, 141.98)), auction = FALSE, timestamp = "15:50:39", 
#     symbol = "SPY", trades = list(c("15:50:29", "400", "141.9400"
#     ), c("15:50:29", "200", "141.9400"), c("15:50:29", "4700", 
#     "141.9400"), c("15:50:29", "1000", "141.9400"), c("15:50:29", 
#     "500", "141.9400"), c("15:50:29", "700", "141.9400"), c("15:50:29", 
#     "700", "141.9400"), c("15:50:29", "700", "141.9400"), c("15:50:29", 
#     "700", "141.9400"), c("15:50:29", "300", "141.9400")), bids = list(
#         c(43200, 141.93), c(26540, 141.92), c(44200, 141.91), 
#         c(41500, 141.9), c(36800, 141.89)), company = "SPDR S&P 500 ETF TR TR UNIT", 
#     orders = 947488), .Names = c("volume", "tick_type", "asks", 
# "auction", "timestamp", "symbol", "trades", "bids", "company", 
# "orders")), success = TRUE), .Names = c("reload", "data", "success"
# ))
# 
# #dput(jsonbyx)
# jsonbyx <- structure(list(reload = 5000, data = structure(list(volume = 3116428, 
#     tick_type = "", asks = list(c(2600, 141.95), c(8000, 141.96
#     ), c(7600, 141.97), c(24800, 141.98), c(13100, 141.99)), 
#     auction = FALSE, timestamp = "15:50:45", symbol = "SPY", 
#     trades = list(c("15:50:40", "100", "141.9400"), c("15:50:39", 
#     "100", "141.9500"), c("15:50:37", "100", "141.9500"), c("15:50:35", 
#     "100", "141.9400"), c("15:50:34", "500", "141.9500"), c("15:50:34", 
#     "100", "141.9500"), c("15:50:32", "500", "141.9400"), c("15:50:32", 
#     "500", "141.9400"), c("15:50:31", "500", "141.9400"), c("15:50:31", 
#     "500", "141.9400")), bids = list(c(600, 141.94), c(20700, 
#     141.93), c(8300, 141.92), c(14100, 141.91), c(13100, 141.9
#     )), company = "SPDR S&P 500 ETF TR TR UNIT", orders = 408810), .Names = c("volume", 
# "tick_type", "asks", "auction", "timestamp", "symbol", "trades", 
# "bids", "company", "orders")), success = TRUE), .Names = c("reload", 
# "data", "success"))

# Demo ----

#par(mfcol=c(1,2))
#plot(ladder(jsonbzx))
#plot(ladder(jsonbyx))
