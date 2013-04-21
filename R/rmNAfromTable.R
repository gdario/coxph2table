##' Remove NAs from an output table
##'
##' This accessory function simply removes NAs from an output table when
##' they are used as an additional level to a categorical variable.
##' @title Remove NAs from an output table
##' @param xt an output table (data frame or xtable)
##' @return the same table (data frame or xtable) without the rows where
##' \code{NA} appeared.
##' @author Giovanni d'Ario
rmNAfromTable <- function(xt) {
	idxNA <- grep(": NA", rownames(xt))
	return(xt[-idxNA,])
}

