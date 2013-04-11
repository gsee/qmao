\name{get_div}
\alias{get_div}
\alias{get_spl}
\title{get dividend or split data}
\usage{
  get_div(Symbol, force = FALSE, silent = TRUE)

  get_spl(Symbol, force = FALSE, silent = TRUE)
}
\arguments{
  \item{Symbol}{name of instrument}

  \item{force}{if TRUE, data will be downloaded from yahoo
  even if it already exists in the instrument object.
  Downloaded data will overwrite data in instrument object,
  if any. (FALSE)}

  \item{silent}{silence warning when instrument is not
  defined (TRUE)}
}
\value{
  whatever is in the 'div' or 'spl' slot of the instrument.
  probably xts data
}
\description{
  find or download dividend and split data
}
\details{
  get data that is stored in the 'div' or 'spl' slot of an
  instrument. If none exists there, it will be downloaded
  from yahoo.  If the instrument exists, but it did not
  have a 'div' or 'spl' slot, the newly downloaded data
  will be stored there.

  If the instrument has an \code{identifier} named "yahoo",
  that ticker will be used in the \code{getDividends} or
  \code{getSplits} call.
}
\examples{
\dontrun{
get_div("SPY", silent=FALSE)
stock("SPY", currency("USD"))
get_div("SPY", silent=FALSE) # will also store in .instrument$SPY$div
getInstrument("SPY")$div
getInstrument("SPY")$
get_div("SPY") # no download...gets data from .instrument envir
get_div("SPY", force=TRUE) # downloads from yahoo

# A non-U.S. stock
# get_div("BMW")  # this doesn't work because BMW is not a U.S. stock
stock("BMW", currency("EUR"), identifiers=list(yahoo="BMW.DE"))
get_div("BMW") # uses the yahoo identifier BMW.DE
}
}

