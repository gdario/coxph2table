\name{multivHR}
\alias{multivHR}
\title{Performs the multivariate version of \code{coxph2table}.}
\usage{
  multivHR(formula = NULL, data = NULL, ...)
}
\arguments{
  \item{formula}{formula describing the multivariate model
  to be fitted.}

  \item{data}{data frame containing the covariates that
  appear in the multivariate model.}

  \item{...}{optional arguments}
}
\value{
  a data frame containing the results of the multivariate
  Cox regression to be further processed by
  \code{coxph2table}/
}
\description{
  Performs the multivariate version of \code{coxph2table}.
}
\author{
  Giovanni d'Ario
}

