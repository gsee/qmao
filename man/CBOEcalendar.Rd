\name{CBOEcalendar}
\alias{CBOEcalendar}
\alias{CFEcalendar}
\title{View the CBOE Expiration Calendar}
\usage{
  CBOEcalendar(year = format(Sys.Date(), "\%Y"), show =
  c("pdf", "webpage"))

  CFEcalendar(year = format(Sys.Date(), "\%Y"), show =
  c("pdf", "webpage"))
}
\arguments{
  \item{year}{4 digit year of the calendar that you would
  like to view. Defaults to the current year as determined
  by \code{Sys.Date()}}

  \item{show}{what to show. Either \dQuote{pdf} or
  \dQuote{webpage}. (Alternatively, can be be numeric: 1
  for \dQuote{pdf}, 2 for \dQuote{webpage})}
}
\value{
  called for side-effect
}
\description{
  Download and view the CBOE Expiration Calendar for a
  given year in pdf format, or view the pdf on the web
  without downloading it.
}
\examples{
\dontrun{
CBOEcalendar() #This year's calendar in your pdf viewer.
CBOEcalendar(2010, show='web') #open webpage with 2010 calendar
}
}
\author{
  Garrett See
}

