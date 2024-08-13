source(here::here("dev", "Arith.R"))

Kronecker <- function(X, Y, ...){
    if (!inherits(X, "symbolicMatrix")){
        stop(deparse(substitute(x)),
             " is not of class 'symbolicMatrix'")
    }
    if (!inherits(Y, "symbolicMatrix")){
        stop(deparse(substitute(y)),
             " is not of class 'symbolicMatrix'")
    }
    dimX <- Dim(X)
    Xmat <- getBody(X)
    lst.row <- vector('list', dimX[1])
    dimY <- Dim(Y)
    zero.Y <- matrix('0', nrow=dimY[1], ncol=dimY[2])
    Ymat <- getBody(Y)
    zero.Y.ind <- Ymat == '0'
    for(i in seq_len(dimX[1])){
        lst <- vector('list', dimX[2])
        for(j in seq_len(dimX[2])){
            e <- symbolicMatrix(Xmat[i,j], nrow=1, ncol=1)
            e.body <- getBody(e)[1,1]
            e.convert <- type.convert(e.body, as.is = TRUE)
            lst[[j]] <- if(is.numeric(e.convert) &&
                           isTRUE(all.equal(e.convert, 0))){
                zero.Y
            } else {
                out <- matrix(paste0(e.body, ' \\cdot ', Ymat),
                              nrow=dimY[1], ncol=dimY[2])
                out[zero.Y.ind] <- '0'
                out
            }
        }
        mats <- lapply(lst, function(x)
            if(is(x, 'symbolicMatrix')) getBody(x) else x)
        lst.row[[i]] <- do.call(cbind, mats)
    }
    Z <- do.call(rbind, lst.row)
    result <- symbolicMatrix(Z, ...)
    result

}

`%Ox%` <- function(e1,e2){
    UseMethod("%Ox%")
}

`%Ox%.symbolicMatrix` <- function(e1, e2){
    if (!inherits(e2, "symbolicMatrix")){
        stop(deparse(substitute(e2)),
             " is not of class 'symbolicMatrix'")
    }
    wrapper <- getWrapper(e1)
    result <- Kronecker(e1, e2)
    matrix <- sub("begin\\{pmatrix\\}", wrapper[1], getLatex(result))
    matrix <- sub("end\\{pmatrix\\}", wrapper[2], matrix)
    result$matrix <- matrix
    result$wrapper <- wrapper
    result
}

if(FALSE){
    X <- symbolicMatrix(matrix(1:6, 2, 3), matrix="bmatrix")
    Y <- symbolicMatrix(matrix(10*(1:6), 3, 2), matrix="bmatrix")
    a <- symbolicMatrix('a', nrow=1, ncol=1)
    I3 <- symbolicMatrix(diag(3))
    X
    Y
    a
    I3

    Kronecker(a, X)
    Kronecker(X, a)
    Kronecker(X, Y)
    Kronecker(I3, X)
    Kronecker(I3, X, sparse = TRUE)

    I3 %Ox% X
    a %Ox% I3

}
