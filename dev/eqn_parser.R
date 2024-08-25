eqn_parser <- function(string,
                       `**`="boldsymbol", `*`="mathcal", ...){

    # `*` -> \times
    if(grepl('`*`', string))
        string <- gsub('`*`', "\\times ", string, fixed=TRUE)

    # %n -> \\\\ \n
    if(grepl('%n', string))
        string <- gsub('%n', "\\\\ ", string, fixed=TRUE)

    # **.**
    if(grepl('\\*\\*', string)){
        loc <- gregexpr("\\*\\*", string)[[1L]]
        for(i in loc[seq(length(loc), 2, by=-2)])
            substring(string, i, i+1L) <- '} '
        string <- gsub('} ', '}', string, fixed = TRUE)
        string <- gsub('\\*\\*', replacement = sprintf("\\\\%s{", `**`), string)
    }

    # *.*
    # Note: this would have a problem with A * B
    if(grepl('\\*', string)){
        loc <- gregexpr("\\*", string)[[1L]]
        for(i in loc[seq(length(loc), 2, by=-2)])
            substring(string, i, i+1L) <- '}'
        string <- gsub('\\*', replacement = sprintf("\\\\%s{", `*`), string)
    }

    string
}

if(FALSE){
    s1 <- "e = mc^2"
    s2 <- "**e** = **mc**^2"
    s3 <- "\\mathcal{H}_0 : \\mathbf{C} \\mathbf{B} & = "
    s4 <- "*H*_0 : *C B* & = "
    s5 <- "*H*_0 : *C* *B* & = "
    s6 <- "*H*_0 : %n *C* *B* & = "
    s7 <- "*H*_0 : *C* `*` *B*  = "
    s8 <- "*H*_0 : **C** **B** & = "

    identical(eqn_parser(s1), s1)
    eqn_parser(s2)
    eqn_parser(s2, `**` = 'mathbf')
    identical(eqn_parser(s3), s3)
    eqn_parser(s4)
    eqn_parser(s5)
    eqn_parser(s6)
    eqn_parser(s7)
    identical(eqn_parser(s8, `**` = 'mathbf'), s3)
}




sprintEqn <- function(string, mats, ...){
    string <- paste0(string, ' ')
    dots <- list(...)
    forms <- formals(eqn_parser)
    matched <- intersect(names(forms), names(dots))
    forms[matched] <- dots[matched]
    string <- do.call(eqn_parser, c(string, forms))
    bodies <- lapply(mats, getLatex)
    bodies <- lapply(bodies, \(x) gsub("\\", "\\\\", x, fixed=TRUE))
    nms <- names(bodies)
    for(i in seq_len(length(nms)))
        string <- gsub(paste0('%', nms[i], " "), bodies[[i]], string)
    do.call(Eqn, c(string=string, dots[setdiff(names(dots), names(forms))]))
}

if(FALSE){

    # from vignette
    Eqn("\\mathbf{X} & = \\mathbf{U} \\mathbf{\\Lambda} \\mathbf{V}^\\top",
        Eqn_newline(),
        ' & =',
        latexMatrix("u", "n", "k"),
        latexMatrix("\\lambda", "k", "k", diag=TRUE),
        latexMatrix("v", "k", "p", transpose = TRUE),
        align=TRUE)

    sprintEqn("**X** & = **U \\Lambda V**^\\top %n
                     & =  %U    %L    %V",
              list(U=latexMatrix("u", "n", "ks"),
                   V=latexMatrix("v", "k", "p", transpose = TRUE),
                   L=latexMatrix("\\lambda", "k", "k", diag=TRUE)),
              align=TRUE, label='eq:svd', `**`='mathbf')

    # next
    A <- latexMatrix(matrix(c(1, -3, 0, 1), 2, 2))
    B <- latexMatrix(matrix(c(5, 3, -1, 4), 2, 2))
    C <- latexMatrix(symbol="c", 2, 3)
    D <- latexMatrix(symbol="d", 2, 3)

    Eqn("\\mathbf{A} + \\mathbf{B} =", A, " + ", B, " = ", A + B, " = ", as.double(A + B))

    sprintEqn("**A** + **B** = %A + %B = %AB = %C",
              list(A=A, B=B, AB=A + B, C=latexMatrix(as.double(A+B))))

    # last
    (C <- latexMatrix(matrix(c(0, 1, 0, 0,
                               0, 0, 1, 0), nrow=2, byrow=TRUE),
                      matrix = "bmatrix"))
    B0 <- latexMatrix('\\beta', ncol = 3, nrow=2, comma=TRUE, prefix.col = 'y_')
    Eqn("\\mathcal{H}_0 : \\mathbf{C} \\mathbf{B} & = ",
        C, B,
        Eqn_newline(),
        '& =',
        B0,
        "= \\mathbf{0}_{(2 \\times 3)}",
        align=TRUE)

    sprintEqn("*H*_0 : **C B** & = %C %B \\\\
                               & = %B0 = **0**_{(2 \\times 3)} ",
              list(C=C, B=B, B0=B0), `**`='mathbf', align=TRUE)

}
