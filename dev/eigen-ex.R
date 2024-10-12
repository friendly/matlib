# Aligning symbolic and numeric matrices
# Display the eigen decomp of a matrix, with an example

library(matlib)

data(workers, package = "matlib")   
head(workers)

vars <- c("Experience", "Income")
# covariance matrix & mean
mu <- colMeans(workers[, vars]) |> print()
S <- cov(workers[, vars]) |> print()

# eigenvalues and eigenvectors
S.eig <- eigen(S)
Lambda <- S.eig$values |> print()
V <- S.eig$vectors |> print()


# align V Lambda V' using \phantom{}
options(digits = 4)
rownames(S) <- colnames(S) <- c("\\small \\text{Exp}", 
                                "\\small \\text{Inc}")
spacer <- "\\phantom{00000000000000}"
Eqn("\\mathbf{S} & = \\mathbf{V}", spacer,
    "\\mathbf{\\Lambda}", spacer,  
    "\\mathbf{V}^\\top", Eqn_newline(),
    latexMatrix(S), "& =", 
    latexMatrix(V), "  ", diag(Lambda), "  ", latexMatrix(V, transpose=TRUE),
    align = TRUE)

# NB: I was surprised that row/col labels for S now appear by default.
#   Also, they are in math font rather than \text{}
#   One idea for improving bordered matrices alignment might be to use \small{} or other reduced size

# What's wrong here?
spacer <- " & "
Eqn("\\mathbf{S} & = \\mathbf{V}", spacer,
    "\\mathbf{\\Lambda}", spacer,  
    "\\mathbf{V}^\\top", Eqn_newline(),
    latexMatrix(S), "& =", 
    latexMatrix(V), "  ", diag(Lambda), "  ", latexMatrix(V, transpose=TRUE),
    align = TRUE)

