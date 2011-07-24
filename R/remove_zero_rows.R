#' Remove rows where specified column(s) has zero value
#' @param Symbols names of instruments
#' @param columns which columns to require to have positive, non-zero values ('Volume')
#' @param env where to find the data. (.GlobalEnv)
#' @param store should the data be overwritten? Must be \code{TRUE} if \code{length(Symbols) > 1}. If \code{FALSE} the cleaned data will be returned.
#' @return usually called for side-effect, returning only the symbol names. If \code{store=FALSE} the cleaned data is returned.
#' @author Garrett See
#' @examples
#' \dontrun{
#' get data for all big Vix futures that expire in 2010
#' getSymbols('VX',Month=1:12,Year=2010, require.volume=FALSE, src='cfe')
#' head(VX_F10); tail(VX_F10)
#' remove_zero_rows('VX_F10','Settle') #require 'Settle' column to be positive
#' head(VX_F10); tail(VX_F10) #Last row has zeros for everything except Settle.
#' remove_zero_rows("VX_G10",'Volume')
#' remove_zero_rows("VX_H10",c("Close","Settle","Volume"))
#' remove_zero_rows(c("VX_J10","VX_K10","VX_M10"),'Volume')
#' }
#' @export
remove_zero_rows <- function(Symbols, columns='Volume', 
                                env=.GlobalEnv, store=TRUE) {
    if (length(Symbols > 1) && !store) stop("'store' must be TRUE if there is more than one 'Symbol'")
    symout <- NULL
    for (symb in Symbols) {
        x <- try(get(symb, pos=env),silent=TRUE)
        if (!inherits(x,'try-error')) {
            for (column in columns) {
                if (!identical(integer(0), grep(column, colnames(x), ignore.case = TRUE))) {
                    cdat <- x[, grep(column,colnames(x),ignore.case=TRUE)]
                    x <- x[cdat > 0]
                }            
            }
            if(store) assign(symb,x,pos=env)
            symout <- c(symout, symb)        
        }
    }
    if(!store) x
    else symout
}
