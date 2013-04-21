\name{rmNAfromTable}
\alias{rmNAfromTable}
\title{Remove NAs from an output table}
\usage{
  rmNAfromTable(xt)
}
\arguments{
  \item{xt}{an output table (data frame or xtable)}
}
\value{
  the same table (data frame or xtable) without the rows
  where \code{NA} appeared.
}
\description{
  Remove NAs from an output table
}
\details{
  This accessory function simply removes NAs from an output
  table when they are used as an additional level to a
  categorical variable.
}
\author{
  Giovanni d'Ario
}

