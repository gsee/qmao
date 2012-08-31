\name{dlPowerShares}
\alias{dlPowerShares}
\title{download CSVs from PowerShares.com}
\usage{
  dlPowerShares(base.url = "http://www.invescopowershares.com/products/",
    event.target = "ctl00$FullPageOverrideContent$LinkButton1",
    action = "")
}
\arguments{
  \item{base.url}{the non-product-specific url}

  \item{event.target}{This is the first string inside of
  the javascript:__doPostBack() function that you will get
  when you right click the \dQuote{Download} link and
  \dQuote{Copy Link Address}}

  \item{action}{the product-specific part of the action
  url}
}
\value{
  a csv downloaded from the PowerShares website
}
\description{
  download CSVs from PowerShares.com
}
\author{
  Garrett See with help from a user that requested to
  remain anonymous on \url{http://stackoverflow.com}
}
\references{
  \url{http://www.invescopowershares.com/products/}
}

