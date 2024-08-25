eqn_parser <- function(string,
                       `**`="boldsymbol", `*`="mathcal", ...){

    # `*` -> \times
    if(grepl('`*`', string))
        string <- gsub('`*`', "\\times ", string, fixed=TRUE)

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
    # s6 <- "*H*_0 : %n *C* *B* & = "
    s7 <- "*H*_0 : *C* `*` *B*  = "
    s8 <- "*H*_0 : **C** **B** & = "

    identical(eqn_parser(s1), s1)
    eqn_parser(s2)
    eqn_parser(s2, `**` = 'mathbf')
    identical(eqn_parser(s3), s3)
    eqn_parser(s4)
    eqn_parser(s5)
    # eqn_parser(s6)
    eqn_parser(s7)
    identical(eqn_parser(s8, `**` = 'mathbf'), s3)
}

#' String formatting for LaTeX Equations
#'
#' @param string character vector indicating the structure of the
#'   LaTeX output equation. Substitutions are specified using the
#'   \code{%} character followed by the name of the object
#'   (e.g., \code{"X = %A"} substitutes the information in the object
#'   \code{A})
#'
#'   Additionally, \code{markdown = TRUE}
#'   markdown syntax for bold fonts (\code{**}) and
#'   italicizes (\code{*}) can be used for wrapping text elements with
#'   \code{boldsymbol} and \code{mathcal} blocks, respectively.
#'   These wrappers can be overwritten by matching the operator specification
#'   in \code{...}; for example, using \code{mathbf} for bold text requires
#'   passing \code{`**` = 'mathbf'}
#' @param subs a named \code{list} containing the information for the
#'   \code{%} indicators in \code{string}. Can be either a
#'   \code{character} vector, a \code{matrix}, or a \code{latexMatrix}
#' @param mat_args list of arguments to be passed to \code{\link{latexMatrix}} to change the
#'   properties of the \code{matrix} input objects. Note that these inputs are used globally, and apply to
#'   each \code{matrix} objects supplied. If further specificity is required create
#'   \code{\link{latexMatrix}} objects directly.
#' @param markdown logical; use asterisk wrappers in a
#'  way similar to markdown text? Note that use of this option disables
#'  the standard use of the asterisk, though it can be included via \code{`*`}
#' @param ... additional arguments to be passed to \code{\link{Eqn}} and
#'   \code{eqn_parser}
#'
sprintEqn <- function(string, subs = list(), mat_args = list(),
                      markdown = TRUE, ...){
    dots <- list(...)
    if(markdown){
        string <- paste0(string, ' ')
        forms <- formals(eqn_parser)
        matched <- intersect(names(forms), names(dots))
        forms[matched] <- dots[matched]
        string <- do.call(eqn_parser, c(string, forms))
    }
    penv <- parent.frame()
    sapply(names(penv), \(x, sub_names){
        if(isTRUE(grepl(paste0('%', x), string)) &&
                  !(x %in% sub_names)){
            subs[[x]] <<- penv[[x]]
        }
    }, sub_names=names(subs))
    bodies <- lapply(subs, \(x) if(inherits(x, 'latexMatrix')){
        getLatex(x)
    } else if(is.matrix(x)){
        getLatex(do.call(latexMatrix, c(list(symbol=x), mat_args)) )
    } else {
        x
    })
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
                     & =  %U    %L    %V",        ## output structure
              list(n=Eqn_newline(),               ## % macro elements
                   U=latexMatrix("u", "n", "ks"),
                   V=latexMatrix("v", "k", "p", transpose = TRUE),
                   L=latexMatrix("\\lambda", "k", "k", diag=TRUE)),
              align=TRUE, label='eq:svd', `**`='mathbf')

    # next
    A <- latexMatrix(aa <- matrix(c(1, -3, 0, 1), 2, 2))
    B <- latexMatrix(bb <- matrix(c(5, 3, -1, 4), 2, 2))
    C <- latexMatrix(symbol="c", 2, 3)
    D <- latexMatrix(symbol="d", 2, 3)

    Eqn("\\mathbf{A} + \\mathbf{B} =", A, " + ", B, " = ", A + B, " = ", as.double(A + B))

    sprintEqn("**A** + **B** = %A + %B = %AB = %C",
              list(A=A, B=B, AB=A + B, C=latexMatrix(as.double(A+B))))
    sprintEqn("**A** + **B** = %A + %B = %AB = %C",
              list(A=A, B=B, AB=A + B, C=latexMatrix(as.double(A+B))), `**`="mathbf")

    # search in parent envir if not listed
    AB <- A + B
    C <- aa + bb
    sprintEqn("**A** + **B** = %A + %B = %AB = %C")

    # pass global constructor args for matricies to be built
    sprintEqn("**A** + **B** = %A + %B = %AB = %C",
              mat_args = list(matrix='bmatrix'))

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
                               & = %B0 = **0**_{(2 `*` 3)}",
              list(C=C, B=B, B0=B0), `**`='mathbf', align=TRUE)

}
