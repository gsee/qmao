\name{remove_zero_rows}
\alias{remove_zero_rows}
\title{Remove rows where specified column(s) has zero value...}
\usage{
  remove_zero_rows(Symbols, columns = "Volume",
    env = .GlobalEnv, store = TRUE)
}
\arguments{
  \item{Symbols}{names of instruments}

  \item{columns}{which columns to require to have positive,
  non-zero values ('Volume')}

  \item{env}{where to find the data. (.GlobalEnv)}

  \item{store}{should the data be overwritten? Must be
  \code{TRUE} if \code{length(Symbols) > 1}. If
  \code{FALSE} the cleaned data will be returned.}
}
\value{
  usually called for side-effect, returning only the symbol
  names. If \code{store=FALSE} the cleaned data is
  returned.
}
\description{
  Remove rows where specified column(s) has zero value
}
\examples{
\dontrun{
get data for all big Vix futures that expire in 2010
getSymbols('VX',Month=1:12,Year=2010, require.volume=FALSE, src='cfe')
head(VX_F10); tail(VX_F10)
remove_zero_rows('VX_F10','Settle') #require 'Settle' column to be positive
head(VX_F10); tail(VX_F10) #Last row has zeros for everything except Settle.
remove_zero_rows("VX_G10",'Volume')
remove_zero_rows("VX_H10",c("Close","Settle","Volume"))
remove_zero_rows(c("VX_J10","VX_K10","VX_M10"),'Volume')
}
}
\author{
  Garrett See
}

