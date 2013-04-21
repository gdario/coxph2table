\name{univHR}
\alias{univHR}
\title{Univariate form of \code{coxph2table}.}
\usage{
  univHR(formula = NULL, data = NULL, ...)
}
\arguments{
  \item{formula}{Formula indicating the type of model to be
  fitted.}

  \item{data}{data frame containing the clinical
  information.}

  \item{...}{optional arguments.}
}
\value{
  a data frame to with the estimates and the confidence
  intervals from a univariate Cox regression
}
\description{
  Univariate form of \code{coxph2table}.
}
\details{
  This function performs the univariate Cox regression
  returning the output in one of the chosen formats.
}
\author{
  Giovanni d'Ario
}

