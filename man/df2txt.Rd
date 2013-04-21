\name{df2txt}
\alias{df2txt}
\title{Convert a data frame into a text table}
\usage{
  df2txt(outTable, Digits = 2, file = "")
}
\arguments{
  \item{outTable}{a data frame, output of
  \code{coxph2table}}

  \item{Digits}{integer, the number of digits that should
  appear in the table.}

  \item{file}{full path to the file where the text file
  should be saved}
}
\value{
  a text file containing the output of \code{coxph2table}.
}
\description{
  Convert a data frame into a text table
}
\details{
  This function takes a data frame produced by
  coxph2table.R and transforms it into a text table
  suitable for MS Excel conversion
}
\author{
  Giovanni d'Ario
}

