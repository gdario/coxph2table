##' Convert from simple to journal-like format
##'
##' Transforms a data frame with the HR and the 95% CI on separate
##' columns in a data frame in the "journal" format.
##' @title Convert from simple to journal-like format
##' @param x data frame, the return value of a call to univHR or
##' multivHR
##' @param Digits integer indicating the number of significant digits that
##' must appear in the HR (Lower - Upper) terms.
##' @return data frame containing the HR and the 95% CI in one single
##' column
##' @author Giovanni d'Ario
simple2journal <- function(x, Digits=Digits) {
	idx <- match(c("HR", "Lower C.I.", "Upper C.I."), names(x))
	x[, idx] <- round(x[, idx], digits = Digits)
	hr.ci <- paste(x[["HR"]], " (", x[["Lower C.I."]], " - ",
			x[["Upper C.I."]], ")", sep = "")
	
	dF <- data.frame("HR (95% CI)" = hr.ci,
			"p-value" = x[["p-value"]],
			stringsAsFactors = FALSE,
			check.names = FALSE)
	rownames(dF) <- rownames(x)
	return(dF)
}

