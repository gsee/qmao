\name{convert.illions}
\alias{convert.illions}
\title{Convert string to number}
\usage{
  convert.illions(x)
}
\arguments{
  \item{x}{string of numbers and a single letter e.g.
  "2.13B", "4T"}
}
\value{
  numeric
}
\description{
  Convert a string such as "1.2M" to a number 1200000
}
\examples{
convert.illions("300M")
convert.illions("1.453T")
}
\author{
  Garrett See
}

