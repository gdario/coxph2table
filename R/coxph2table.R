##' Fit and pretty-print uni or multivariate Cox regressions
##'
##' This function fits the uni or multivariate Cox regression specified
##' in the formula and returns an output that is suitable for pubblication.
##' The output can be in LaTeX, in text or a data frame for further manual
##' modification. There are two formats available, one of which is closer to
##' what is usually found in publications. If categorical covariates contain NAs as
##' additional levels, the \code{rmNA} allows to remove the corresponding rows
##' from the output. The number of digits can be set by the user, and the text
##' output can be saved on a file (default is to print it to screen).
##' \strong{Note;} the user must specify whether the model is univariate or
##' multivariate, 
##' since the formula can contain multiple covariates even in univariate 
##' models. In this case each covariate will be fit independently in a
##' univariate model, but the results will be presented in the same table for 
##' all covariates.
##' @title Fit and pretty-print uni or multivariate Cox regressions
##' @param formula formula object specifying the uni or multivariate model to
##' be fitted.
##' @param data a data frame containing the covariates
##' @param output character string indicating the type of output.
##' Possible values are \text{latex}, \code{data.frame} and \code{text}.
##' The default value is \code{latex}.
##' @param Format character string indicating the type of format. Possible values
##' are \code{journal} and \code{simple}. The first one is the default.
##' @param rmNA logical, whehter rows corresponding to NA-categories should be
##' removed. Default is \code{FALSE}.
##' @param Digits integer, the number of floating point digits in the output.
##' The default value is 2.
##' @param file full path to the file where the text output should be saved. The
##' default value is "", i.e. the standard output.
##' @param type character string, indicates the type of model. Possible values are
##' \code{univariate} and \code{multivariate}. \strong{Note} this argument must
##' always be set, since it decides the way the output will be presented.
##' @param caption Character string (possibly multiline, glued together with
##' \code{paste}). This is the caption that will appear in the LaTeX output.
##' @param label character string representing the label used in LaTeX for
##' referencing
##' @param ... optional arguments
##' @return either some LaTeX code, a text output or file, or a data frame, depending
##' on the value of the \code{output} parameter
##' @author Giovanni d'Ario
##' @export
coxph2table <- function(formula=NULL,
                        data=NULL,
                        output=c("latex",
                            "data.frame",
                            "text"),
                        Format=c("journal",
                            "simple"),
                        rmNA=FALSE,
                        Digits=2,
                        file="",
                        type=c("univariate",
                            "multivariate"),
                        caption=NULL,
                        label=NULL,
                        ...) {

    ## Match the arguments to be passed to the sub-functions
    type <- match.arg(type)
    output <- match.arg(output)
    Format <- match.arg(Format)

    if(is.null(type))
        stop("Error: you MUST specify if the model is univariate or multivariate")
    
    if(type == "univariate")               
        outTable <- univHR(formula = formula,
                           data = data, ...)                           
    else
        if(type == "multivariate") 
            outTable <- multivHR(formula = formula,
                                 data = data, ...)
    
    if(rmNA)
        outTable <- rmNAfromTable(outTable)
    
    if(Format == "journal") {
        outTable <- simple2journal(outTable, Digits = Digits)
        display <- c("s", "s", "g")
    } else {
        display <- c("s", "f", "f", "f", "g")
    }
    
    if(output == "text") 
        write.table(outTable, file = file, sep = "\t", quote = FALSE)
	
    else if(output == "latex")
        outTable <- xtable(outTable,
                           display = display,
                           caption = caption,
                           label = label)
    return(outTable)
}
