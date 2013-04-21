##' Univariate form of \code{coxph2table}.
##'
##' This function performs the univariate Cox regression returning
##' the output in one of the chosen formats.
##' @title Univariate form of \code{coxph2table}.
##' @param formula Formula indicating the type of model to be fitted.
##' @param data data frame containing the clinical information.
##' @param ... optional arguments.
##' @return a data frame to with the estimates and the confidence intervals
##' from a univariate Cox regression
##' @author Giovanni d'Ario
univHR <- function(formula=NULL,
		data=NULL,
		...) {
	
	## Load the necessary libraries
	if(!require(survival))
		stop("ERROR: I cannot find the survival library.")
	if(!require(xtable))
		stop("ERROR: I cannot find the xtable library.")
	
	mf <- model.frame(formula = formula, data = data, na.action = NULL)
	
	survData <- mf[[1]]
	covariates <- names(mf)[-1]
	
	modelList <- list()
	outTable <- data.frame()
	
	for(i in seq_along(covariates)) {
		f = paste("survData", covariates[i], sep = "~")
		f <- as.formula(f)
		m <- coxph(formula = f, data = mf, ...)
		sm <- summary(m)
		ci <- sm$conf.int
		cf <- sm$coef
		xt <- cbind(ci[, match(c("exp(coef)",
										"lower .95",
										"upper .95"),
								colnames(ci)), drop = FALSE],
				cf[, match("Pr(>|z|)", colnames(cf)), drop = FALSE])
		modelList[[i]] <- m
		
		rn <- rownames(xt)
		
		if(is.factor(data[[covariates[i]]])) {
			rn <- paste(paste(toupper(covariates[i]), ":",
							levels(data[[covariates[i]]])[-1]),
					"vs", levels(data[[covariates[i]]])[1])
			rownames(xt) <- rn
		} else {
			rn <- toupper(covariates[i])
			rownames(xt) <- rn
		}
		
		outTable <- rbind(outTable, xt)
	}
	colnames(outTable) <- c("HR", "Lower C.I.", "Upper C.I.", "p-value")
	
	return(outTable)
}

