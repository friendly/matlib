eqn_parser <- function(string,
                       `**`="mathbf", `*`="mathcal",
                       GreekBS = TRUE, ...){

    # `*` -> \times
    if(grepl('`*`', string))
        string <- gsub('`*`', "\\times ", string, fixed=TRUE)

    if(GreekBS){
        for(symb in GreekLaTeXSymbols()){
            bsymb <- paste0('**', symb, "**")
            replace <- paste0('\\boldsymbol{', symb, "}")
            string <- gsub(bsymb, replace, string, fixed=TRUE)
        }
    }

    # **.**
    if(grepl('\\*\\*', string)){
        loc <- gregexpr("\\*\\*", string)[[1L]]
        for(i in loc[seq(length(loc), 2, by=-2)])
            substring(string, i, i+1L) <- '}#'
        string <- gsub('}#', "}", string)
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

GreekLaTeXSymbols <- function() paste0("\\", c("alpha", "nu", "beta", "xi", "Xi",
                             "gamma", "Gamma","delta", "Delta",
                             "pi", "Pi", "epsilon", "varepsilon",
                             "rho", "varrho", "zeta", "sigma", "Sigma",
                             "eta", "tau", "theta", "vartheta",
                             "Theta", "upsilon", "Upsilon", "iota",
                             "phi", "varphi", "Phi", "kappa", "chi",
                             "lambda", "Lambda", "psi", "Psi",
                             "mu", "omega", "Omega"))

if(FALSE){
    s1 <- "e = mc^2"
    s2 <- "**e** = **mc**^2"
    s3 <- "\\mathcal{H}_0 : \\mathbf{C} \\mathbf{B} & = "
    s4 <- "*H*_0 : *C B* & = "
    s5 <- "*H*_0 : *C* *B* & = "
    # s6 <- "*H*_0 : %n *C* *B* & = "
    s7 <- "*H*_0 : *C* `*` *B*  = "
    s8 <- "*H*_0 : **C** **B** & = "
    s9 <- "**U** **\\Lambda** **V**^\\top"
    s10 <- "\\mathbf{U} \\boldsymbol{\\Lambda} \\mathbf{V}^\\top"

    identical(eqn_parser(s1), s1)
    eqn_parser(s2)
    eqn_parser(s2, `**` = 'boldsymbol')
    identical(eqn_parser(s3), s3)
    eqn_parser(s4)
    eqn_parser(s5)
    # eqn_parser(s6)
    eqn_parser(s7)
    identical(eqn_parser(s8), s3)
    eqn_parser(s9)
    identical(eqn_parser(s9), s10)
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
#'   \code{mathbf}/\code{boldsymbol} (for detected Greek symbols)
#'   and \code{mathcal} blocks, respectively.
#'   These wrappers can be overwritten by matching the operator specification
#'   in \code{...}; for example, using \code{boldsymbol} for bold text requires
#'   passing \code{`**` = 'boldsymbol'}
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
#'   the internal equation parser if \code{markdown = TRUE}
#'
#' @examples
#'
#' # no formatting; equivalent to Eqn()
#' printEqn("\\mathcal{H}_0 : \\mathbf{C} \\mathbf{B}")
#'
#' # markdown-style formatting
#' printEqn("*H*_0 : **C** **B**")
#'
#' # preview (caught from Eqn())
#' printEqn("*H*_0 : **C** **B**", preview = TRUE)
#'
#' # Bold Greek letters use boldsymbol
#' printEqn("*H*_0 : **\\Lambda** **B**")
#' printEqn("*H*_0 : **\\Lambda** **B**", GreekBS = FALSE) # disabled
#'
#' # expressions with % substitutions
#' c <- matrix(c(0, 1, 0, 0,
#'               0, 0, 1, 0), nrow=2, byrow=TRUE)
#' C <- latexMatrix(c, matrix = "bmatrix")
#' B <- latexMatrix('\\beta', ncol = 3, nrow=4,
#'                  comma=TRUE, prefix.col = 'y_',
#'                  zero.based=c(TRUE, FALSE))
#' B0 <- latexMatrix('\\beta', ncol = 3, nrow=2, comma=TRUE,
#'                   prefix.col = 'y_')
#'
#' # specify how complete LaTeX equation should appear
#' printEqn("**C** + **C** = %C + %C = %CC = %D",
#'          list(C=C, CC = C + C, D = c + c))
#'
#' # align with &
#' printEqn("*H*_0 : **C B** & = %C %B \\\\
#'                           & = %B0 = **0**_{(2 \\times 3)}",
#'          list(C=C, B=B, B0=B0), align=TRUE)
#'
#' # If not specified in list will search in parent environment
#' printEqn("*H*_0 : **C B** & = %C %B \\\\
#'                           & = %B0 = **0**_{(2 \\times 3)}",
#'          align=TRUE)
#'
#' # Change ** use LaTeX "boldsymbol" instead of "mathbf" and
#' #  use %n to indicate newline
#' printEqn("*H*_0 : **C B** & = %C %B %n
#'                           & = %B0 = **0**_{(2 \\times 3)}",
#'          list(n="\\\\", C=C, B=B, B0=B0),
#'          `**`='boldsymbol', align=TRUE)
#'
printEqn <- function(string, subs = list(), mat_args = list(),
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

    # TODO worth detecting Greek letters to use boldsymbol?
    printEqn("**X** & = **U** \\boldsymbol{\\Lambda} **V**^\\top %n
                     & =  %U    %L    %V",        ## output structure
              list(n=Eqn_newline(),               ## % macro elements
                   U=latexMatrix("u", "n", "ks"),
                   V=latexMatrix("v", "k", "p", transpose = TRUE),
                   L=latexMatrix("\\lambda", "k", "k", diag=TRUE)),
              align=TRUE, label='eq:svd')

    # next
    A <- latexMatrix(aa <- matrix(c(1, -3, 0, 1), 2, 2))
    B <- latexMatrix(bb <- matrix(c(5, 3, -1, 4), 2, 2))
    C <- latexMatrix(symbol="c", 2, 3)
    D <- latexMatrix(symbol="d", 2, 3)

    Eqn("\\mathbf{A} + \\mathbf{B} =", A, " + ", B, " = ", A + B, " = ", as.double(A + B))

    printEqn("**A** + **B** = %A + %B = %AB = %C",
              list(A=A, B=B, AB=A + B, C=latexMatrix(as.double(A+B))))

    # search in parent envir if not listed
    AB <- A + B
    C <- aa + bb
    printEqn("**A** + **B** = %A + %B = %AB = %C")

    # pass global constructor args for matricies to be built
    printEqn("**A** + **B** = %A + %B = %AB = %C",
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

    printEqn("*H*_0 : **C B** & = %C %B \\\\
                               & = %B0 = **0**_{(2 \\times 3)}",
              list(C=C, B=B, B0=B0), align=TRUE)

}
