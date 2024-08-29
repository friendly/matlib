#' Add partition lines to LaTeX matrix
#'
#' @param x an object of class 'latexMatrix'
#' @param row vector of row(s) indices to apply a horizontal line to
#' @param col vector of column(s) indices to apply a vertical line to
#'
#' @examples
#'
#' M <- latexMatrix('\\beta', nrow=5, ncol=6)
#' Mp <- addPartitions(M, row=1)
#' Mp
#'
#' addPartitions(M, row=c(2,4))
#' addPartitions(M, col=5)
#' addPartitions(M, col=5, row=1)
#'
addPartitions <- function(x, row, col){
    X <- getBody(x)
    nrow <- Nrow(x)
    ncol <- Ncol(x)
    if(!missing(row)){
        stopifnot(all(row > 0) && all(row < nrow))
        X[row + 1, 1] <- paste("\\hline", X[row + 1, 1])
    }
    if(!missing(col)){
        stopifnot(all(col > 0) && all(col < ncol))
        X[, col] <- paste(X[, col], '\\bigm|')
    }
    X <- latexMatrix(X)
    updateWrapper(X, getWrapper(x))
}

