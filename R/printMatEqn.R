#' Print Matrices or Matrix Operations Side by Side
#'
#' This function is designed to print a collection of matrices, vectors, character strings
#' and matrix expressions side by side. A typical use is to illustrate matrix equations in a compact and
#' comprehensible way.
#'
#' @param ... matrices and character operations to be passed and printed to the console. These
#'        can include named arguments, character string operation symbols (e.g., \code{"+"})
#' @param space amount of blank spaces to place around operations such as \code{"+"},
#'   \code{"-"}, \code{"="}, etc
#' @param tol tolerance for rounding
#' @param fractions logical; if \code{TRUE}, try to express non-integers as rational numbers
#' @return NULL; A formatted sequence of matrices and matrix operations is printed to the console
#' @author Phil Chalmers
#' @export
#' @seealso \code{\link{showEqn}}
#' @examples
#'
#' A <- matrix(c(2, 1, -1,
#'              -3, -1, 2,
#'              -2,  1, 2), 3, 3, byrow=TRUE)
#' x <- c(2, 3, -1)
#'
#' # provide implicit or explicit labels
#' printMatEqn(AA = A, "*", xx = x, '=', b = A %*% x)
#' printMatEqn(A, "*", x, '=', b = A %*% x)
#' printMatEqn(A, "*", x, '=', A %*% x)
#'
#' # compare with showEqn
#' b <- c(4, 2, 1)
#' printMatEqn(A, x=paste0("x", 1:3),"=", b)
#' showEqn(A, b)
#'
#' # decimal example
#' A <- matrix(c(0.5, 1, 3, 0.75, 2.8, 4), nrow = 2)
#' x <- c(0.5, 3.7, 2.3)
#' y <- c(0.7, -1.2)
#' b <- A %*% x - y
#'
#' printMatEqn(A, "*", x, "-", y, "=", b)
#' printMatEqn(A, "*", x, "-", y, "=", b, fractions=TRUE)
#'
printMatEqn <- function(..., space = 1, tol=sqrt(.Machine$double.eps),
					  fractions=FALSE) {

	# get arguments
	args <- list(...)
	chars <- sapply(args, is.character)
	args[chars] <- lapply(args[chars], function(x)
		paste0(rep(' ', space), x, rep(' ', space)))

	# vectors to matrix
	pick <- sapply(args, function(x) !is.matrix(x))
	args[pick] <- lapply(args[pick], as.matrix)

	nrows <- sapply(args, nrow)
	ncols <- sapply(args, ncol)

	mat <- matrix('', max(nrows), sum(ncols))
	tmp <- names(args)
	Call <- as.character(match.call())[-1L]
	if(is.null(tmp)){
		tmp <- Call[1L:length(ncols)]
	} else if(length(tmp[tmp != ""]) != sum(!chars)){
		tmp2 <- tmp
		tmp <- Call[1L:length(ncols)]
		tmp[tmp2 != ""] <- tmp2[tmp2 != ""]
	}
	tmp[chars] <- ' '
	nms <- rep(' ', sum(ncols))
	nms[cumsum(ncols)] <- tmp

	for(i in 1L:length(args)){
		if(!chars[i])
			args[[i]] <- if(!fractions)
				format(formatNumbers(args[[i]], tol=tol, fractions=FALSE))
		 	else as.character(formatNumbers(args[[i]], tol=tol, fractions=TRUE))
	}

	for(i in 1L:max(nrows))
		mat[i, ] <- do.call(c, lapply(args, function(x)
			if(nrow(x) >= i) x[i, ] else rep('', ncol(x))))

	ret <- data.frame(mat, stringsAsFactors = FALSE)
	colnames(ret) <- nms
	print(ret, row.names=FALSE)
}
