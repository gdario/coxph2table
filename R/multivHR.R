##' Performs the multivariate version of \code{coxph2table}.
##'
##' 
##' @title Performs the multivariate version of \code{coxph2table}.
##' @param formula formula describing the multivariate model to be fitted.
##' @param data data frame containing the covariates that appear in the
##' multivariate model.
##' @param ... optional arguments
##' @return a data frame containing the results of the multivariate Cox
##' regression to be further processed by \code{coxph2table}/
##' @author Giovanni d'Ario
multivHR <- function(formula=NULL,
		data=NULL,
		...) {
	
	if(!require(survival))
		stop("multivHR needs that the 'survival' package be installed")
	if(!require(xtable))
		stop("multivHR needs that the 'xtable' package be installed")
	
	mf <- model.frame(formula = formula, data = data)
	survData <- mf[, 1]
	
	## Who's a factor?
	tmpData <- mf[, -1]
	covariates <- names(tmpData)
	idx <- which(sapply(tmpData, is.factor))
	
	## First the factors and then the numerical vars
	tmpData <- cbind(tmpData[idx], tmpData[-idx])
	
	## Do the same with the covariates
	tmpCovariatess <- c(covariates[idx], covariates[-idx])
	
	## Re-index the factors (that are now adjacent)
	idx <- sapply(tmpData, is.factor)
	
	## Get the levels of each factor
	levelsList <- lapply(tmpData[idx], levels)
	
	## Get the reference level and the other ones
	firstLevels <- sapply(levelsList, function(x) return(x[1]))
	otherLevels <- lapply(levelsList, function(x) return(x[-1]))
	
	## How many non-reference levels are there in each factor?
	ll <- sapply(otherLevels, length)
	
	firstLevels <- rep(firstLevels, ll)
	otherLevels <- unlist(otherLevels)
	
	## Create the rownames of the table
	rn <- paste(otherLevels, firstLevels, sep = " vs. ")
	parNames <- toupper(names(tmpData)[idx])
	parNames <- rep(parNames, ll)
	rn <- paste(parNames, rn, sep = " : ")
	rn <- c(rn, toupper(names(tmpData)[!idx]))
	
	tmpCovariatess <- paste(tmpCovariatess, collapse = " + ")
	f <- paste("survData", tmpCovariatess, sep = "~")
	f <- as.formula(f)
	m <- coxph(formula = f, data = tmpData, ...)
	sm <- summary(m)
	ci <- sm$conf.int
	cf <- sm$coef
	outTable <- cbind(ci[, match(c("exp(coef)",
									"lower .95",
									"upper .95"),
							colnames(ci)), drop = FALSE],
			cf[, match("Pr(>|z|)", colnames(cf)), drop = FALSE])
	
	rownames(outTable) <- rn
	colnames(outTable) <- c("HR", "Lower C.I.", "Upper C.I.", "p-value")
	outTable <- as.data.frame(outTable)
	
	return(outTable)
}

