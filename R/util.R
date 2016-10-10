# The parent flag is used to search in the parent envir for suitable definitions.
# Set to TRUE if you want to only use the inputs provided
printMatrix <- function(A, parent = TRUE, fractions = FALSE, latex = FALSE, 
						tol = sqrt(.Machine$double.eps)){
	if(parent){
		envir <- as.environment(-1L)
		if(!is.null(envir$fractions)) fractions <- envir$fractions
		if(!is.null(envir$latex)) latex <- envir$latex
		if(!is.null(envir$tol)) tol <- envir$tol
	}
	if (latex) {
		matrix2latex(A, fractions=fractions, digits = round(abs(log(tol,10))))
	} else {
		if (fractions) print(MASS::fractions(as.matrix(A)))
		else print(round(as.matrix(A), round(abs(log(tol,10)))))
	}
}