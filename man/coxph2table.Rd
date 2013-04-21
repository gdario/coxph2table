\name{coxph2table}
\alias{coxph2table}
\title{Fit and pretty-print uni or multivariate Cox regressions}
\usage{
  coxph2table(formula = NULL, data = NULL,
    output = c("latex", "data.frame", "text"),
    Format = c("journal", "simple"), rmNA = FALSE,
    Digits = 2, file = "", type = NULL, caption = NULL,
    label = NULL, ...)
}
\arguments{
  \item{formula}{formula object specifying the uni or
  multivariate model to be fitted.}

  \item{data}{a data frame containing the covariates}

  \item{output}{character string indicating the type of
  output. Possible values are \text{latex},
  \code{data.frame} and \code{text}. The default value is
  \code{latex}.}

  \item{Format}{character string indicating the type of
  format. Possible values are \code{journal} and
  \code{simple}. The first one is the default.}

  \item{rmNA}{logical, whehter rows corresponding to
  NA-categories should be removed. Default is
  \code{FALSE}.}

  \item{Digits}{integer, the number of floating point
  digits in the output. The default value is 2.}

  \item{file}{full path to the file where the text output
  should be saved. The default value is "", i.e. the
  standard output.}

  \item{type}{character string, indicates the type of
  model. Possible values are \code{univariate} and
  \code{multivariate}. \strong{Note} this argument must
  always be set, since it decides the way the output will
  be presented.}

  \item{caption}{Character string (possibly multiline,
  glued together with \code{paste}). This is the caption
  that will appear in the LaTeX output.}

  \item{label}{character string representing the label used
  in LaTeX for referencing}

  \item{...}{optional arguments}
}
\value{
  either some LaTeX code, a text output or file, or a data
  frame, depending on the value of the \code{output}
  parameter
}
\description{
  Fit and pretty-print uni or multivariate Cox regressions
}
\details{
  This function fits the uni or multivariate Cox regression
  specified in the formula and returns an output that is
  suitable for pubblication. The output can be in LaTeX, in
  text or a data frame for further manual modification.
  There are two formats available, one of which is closer
  to what is usually found in publications. If categorical
  covariates contain NAs as additional levels, the
  \code{rmNA} allows to remove the corresponding rows from
  the output. The number of digits can be set by the user,
  and the text output can be saved on a file (default is to
  print it to screen). \strong{Note;} the user must specify
  whether the model is univariate or multivariate, since
  the formula can contain multiple covariates even in
  univariate models. In this case each covariate will be
  fit independently in a univariate model, but the results
  will be presented in the same table for all covariates.
}
\author{
  Giovanni d'Ario
}

