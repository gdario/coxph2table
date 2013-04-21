##' Convert a data frame into a text table
##'
##' This function takes a data frame produced by coxph2table.R and transforms it
##' into a text table suitable for MS Excel conversion
##' @title Convert a data frame into a text table
##' @param outTable a data frame, output of \code{coxph2table}
##' @param Digits integer, the number of digits that should appear in the table.
##' @param file full path to the file where the text file should be saved
##' @return a text file containing the output of \code{coxph2table}.
##' @author Giovanni d'Ario
df2txt <- function(outTable, Digits=2, file="") {
	rn <- rownames(outTable)
	
	## Make the header
	
	cat(file = file, "Parameter\tHR (95% C.I.)\tp-value\n")
	
	for(i in 1:nrow(outTable)) {
		## Round the HR and its C.I. to the given number of digits
		hr.ci <- round(outTable[i, c("HR", "Lower C.I.",
								"Upper C.I.")], digits = Digits)
		p <- outTable[i, "p-value"]
		cat(file = file, rn[i], "\t", hr.ci[1, 1],
				" (", hr.ci[1, 2], " - ",
				hr.ci[1, 3], ")\t", p, "\n", append = TRUE)
	}
}
