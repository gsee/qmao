\name{Mi}
\alias{As}
\alias{Bi}
\alias{Ch}
\alias{Mi}
\alias{Tr}
\title{Extract Columns of Data from and xts object}
\usage{
  Mi(x, symbol.name = NULL)

  Bi(x)

  As(x)

  Ch(x)

  Tr(x)
}
\arguments{
  \item{x}{a data object with columns containing data to be
  extracted}

  \item{symbol.name}{used only if the Mid column has to be
  calculated for creating the column name.}
}
\value{
  an xts object with the appropriately named column
}
\description{
  Extract "Bid", "Ask", "Mid", "Trade", and "Change"
  columns
}
\details{
  These are similar to the quantmod functions
  \code{\link{Op}}, \code{\link{Hi}}, \code{\link{Lo}},
  \code{\link{Cl}}, \code{\link{Ad}}.  They use grep to
  locate the appropriate columns, and are case insensitive.

  \code{Mi} will calculate the Mid column if it does not
  exist, but Bid and Ask columns do exist.
}

