\name{simple2journal}
\alias{simple2journal}
\title{Convert from simple to journal-like format}
\usage{
  simple2journal(x, Digits = Digits)
}
\arguments{
  \item{x}{data frame, the return value of a call to univHR
  or multivHR}

  \item{Digits}{integer indicating the number of
  significant digits that must appear in the HR (Lower -
  Upper) terms.}
}
\value{
  data frame containing the HR and the 95% CI in one single
  column
}
\description{
  Convert from simple to journal-like format
}
\details{
  Transforms a data frame with the HR and the 95% CI on
  separate columns in a data frame in the "journal" format.
}
\author{
  Giovanni d'Ario
}

