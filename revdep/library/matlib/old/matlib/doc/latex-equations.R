## ----nomessages, echo = FALSE-------------------------------------------------
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE
)
options(digits=4)
par(mar=c(5,4,1,1)+.1)

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----include=FALSE------------------------------------------------------------
library(matlib)

## ----eqn1,results='asis'------------------------------------------------------
latexMatrix() |> Eqn()

## ----eqn2,results='asis'------------------------------------------------------
Eqn("\\mathbf{I}_3 =", latexMatrix(diag(3), matrix="bmatrix"))

## ----eqn3,results='asis'------------------------------------------------------
latexMatrix(matrix(LETTERS[1:4], nrow=1), matrix="Bmatrix") |> Eqn()

## ----eqn4,results='asis'------------------------------------------------------
latexMatrix(matrix(letters[1:3], ncol=1), matrix = "vmatrix") |> Eqn()

## ----eqn5,results='asis'------------------------------------------------------
latexMatrix("\\mathbb{q}", 3, 3, 
            matrix = "bmatrix",
            zero.based = c(TRUE, FALSE), 
            comma=TRUE, 
            exponent="-1") |>
  Eqn()

## ----eval=FALSE---------------------------------------------------------------
#  Eqn("\\mathbf{X} = \\mathbf{U} \\mathbf{\\Lambda} \\mathbf{V}^\\top", label='eq:svd')
#  Eqn("\\mathbf{X} =",
#      latexMatrix("u", "n", "k"),
#      latexMatrix("\\lambda", "k", "k", diag=TRUE),
#      latexMatrix("v", "k", "p", transpose = TRUE), label='eq:svdmats')

## ----eqn-svd,results='asis', echo=FALSE---------------------------------------
Eqn("\\mathbf{X} = \\mathbf{U} \\mathbf{\\Lambda} \\mathbf{V}^\\top", label='eq:svd')
Eqn(latexMatrix("u", "n", "k"),
    latexMatrix("\\lambda", "k", "k", diag=TRUE),
    latexMatrix("v", "k", "p", transpose = TRUE), label='eq:svdmats')

## ----eqn-Axb,symbMat, results='asis'------------------------------------------
Eqn(latexMatrix("a", nrow = "m", ncol="n", matrix="bmatrix"),
    latexMatrix("x", nrow = "n", ncol=1),
    Eqn_hspace(mid='='),
    latexMatrix("b", nrow = "m", ncol=1))

## ----eqn-align,results='asis'-------------------------------------------------
Eqn("\\mathbf{X} & = \\mathbf{U} \\mathbf{\\Lambda} \\mathbf{V}^\\top",
    Eqn_newline(),
    ' & =',
    latexMatrix("u", "n", "k"),
    latexMatrix("\\lambda", "k", "k", diag=TRUE),
    latexMatrix("v", "k", "p", transpose = TRUE),
    align=TRUE)

## ----matrix-arithmetic--------------------------------------------------------
(A <- latexMatrix(matrix(c(1, -3, 0, 1), 2, 2)))
(B <- latexMatrix(matrix(c(5, 3, -1, 4), 2, 2)))
(C <- latexMatrix(symbol="c", 2, 3))
(D <- latexMatrix(symbol="d", 2, 3))

## ----matrix-arithmetic2-------------------------------------------------------
A + B

## ----matrix-arithmetic3-------------------------------------------------------
A - B
-A            # unary minus
2*A           # scalar multiplication
C + D         # sum of symbolics
"\\pi" * C    # everything should be multiplied by pi

## ----scalar-product-displayed, results='asis', echo=FALSE---------------------
Eqn("\\pi" * C)

## ----coerce-to-numeric--------------------------------------------------------
as.numeric(A + B)

## ----matrix-arith-equation, results='asis'------------------------------------
Eqn("\\mathbf{A} + \\mathbf{B} =", A, " + ", B, " = ", A + B)

## ----coerce-to-numeric-with-locals--------------------------------------------
(M <- latexMatrix(matrix(letters[1:9], 3, 3)))
as.double(-2*M, locals=c(a=1, b=0, c=-2, d=-4, e=7, f=-1, g=0, h=4, i=6))

## ----product1, results='asis'-------------------------------------------------
A %*% B

## ----product2, results='asis'-------------------------------------------------
A %*% latexMatrix("b", 2, 2)

## ----multSymbol, eval=FALSE---------------------------------------------------
#  options(latexMultSymbol = "\\times")

## ----transpose----------------------------------------------------------------
D
t(D)
M %*% t(D)

## ----matrix-product-displayed, results='asis', echo=FALSE---------------------
Eqn(M %*% t(D))

## ----determinant--------------------------------------------------------------
A
determinant(A)
M
determinant(M)

## ----determinant-displayed, results='asis', echo=FALSE------------------------
Eqn(determinant(M))

## ----inverse------------------------------------------------------------------
solve(A)

## ----simplify-inverse---------------------------------------------------------
solve(M, simplify=TRUE)

## ----inverse-displayed, results='asis', echo=FALSE----------------------------
Eqn(solve(M, simplify=TRUE))

## ----numeric-inverse----------------------------------------------------------
MASS::fractions(as.double(solve(M), 
                          locals=c(a=1, b=0, c=-2, d=-4, e=7, f=-1, g=0, h=4, i=6)))
MASS::fractions(det(as.double(M, 
                              locals=c(a=1, b=0, c=-2, d=-4, e=7, f=-1, g=0, h=4, i=6))))

## ----linhyp-beta, results='asis'----------------------------------------------
(B <- latexMatrix('\\beta', ncol = 3, nrow=4, 
                 comma=TRUE, prefix.col = 'y_',
                 zero.based=c(TRUE, FALSE)))

## ----linhyp-C, results='asis'-------------------------------------------------
(C <- latexMatrix(matrix(c(0, 1, 0, 0,
                           0, 0, 1, 0), nrow=2, byrow=TRUE), 
                 matrix = "bmatrix"))

## ----linear-hypotheses, results='asis'----------------------------------------
B0 <- latexMatrix('\\beta', ncol = 3, nrow=2, comma=TRUE, prefix.col = 'y_')
Eqn("\\mathcal{H}_0 : \\mathbf{C} \\mathbf{B} & = ",
    C, B,
    Eqn_newline(), 
    '& =',
    B0,
    "= \\mathbf{0}_{(2 \\times 3)}", 
    align=TRUE)

## ----partition1---------------------------------------------------------------
M <- latexMatrix("m", 4, 4)
Mpart <- latexMatrix('\\mathbf{M}', nrow = 2, ncol = 2, comma = TRUE)
Eqn("\\mathbf{M} =", Mpart,
    " =", M)

## ----partition2, results='asis', echo=FALSE-----------------------------------
M <- latexMatrix("m", 4, 4)
Mpart <- latexMatrix('\\mathbf{M}', nrow = 2, ncol = 2, comma = TRUE)
Eqn("\\mathbf{M} =", Mpart,
    " =", M)

## ----partition3---------------------------------------------------------------
M11 <- M[1:2, 1:2] |> print()
M12 <- M[1:2, 3:4]
M21 <- M[3:4, 1:2]
M22 <- M[3:4, 3:4]

## ----partition4---------------------------------------------------------------
rbind(
  cbind(M11, M12),
  cbind(M21, M22)
)

## ----partition5, results='asis'-----------------------------------------------
Eqn(M11, M12,
    Eqn_newline(),
    M21, M22,
    align = TRUE)

## ----partition6, results='asis'-----------------------------------------------
partition(M, rows=2, columns=2)

## ----partition7, results='asis'-----------------------------------------------
partition(M, rows=c(1,3), columns=c(1,3))

## ----partition8, results='asis'-----------------------------------------------
C <- latexMatrix("\\mathbf{C}", 2, 2)
D <- latexMatrix("\\mathbf{D}", 2, 2)
Eqn("\\mathbf{C} + \\mathbf{D} =",
    C, "+", D, "=", 
    C + D)

## ----kronecker1, results='asis'-----------------------------------------------
A <- matrix(1:4, nrow = 2) |> 
  latexMatrix() |> print()
B <- matrix(5:8, nrow = 2) |> 
  latexMatrix() |> print()
kronecker(A, B) |> partition(rows = 2, columns = 2)

## ----kronecker2, results='asis'-----------------------------------------------
Bmat <- latexMatrix('\\mathbf{B}', ncol=1, nrow=1)
KABmat <- kronecker(A, Bmat)
KAB <- kronecker(A, B)

Eqn("\\mathbf{A} \\otimes \\mathbf{B} = &",
    KABmat,
    Eqn_newline(), Eqn_vspace("1.5ex"), "= & ",
    KAB |> partition(rows = 2, columns = 2),
    Eqn_newline(), Eqn_vspace("1.5ex"), "= & ",
    latexMatrix(as.double(KAB)) |> partition(rows = 2, columns = 2),
    align = TRUE)

## ----results='asis'-----------------------------------------------------------
A <- matrix(1:12, nrow=3, ncol=4, byrow = TRUE) / 6
matrix2latex(A, fractions = TRUE, brackets = "b") |> Eqn()

## -----------------------------------------------------------------------------
A <- matrix(paste0('a_', 1:9), 3, 3, byrow = TRUE) |> print()
b <- paste0("\\beta_", 1:3) |> print()

## ----results='asis'-----------------------------------------------------------
matrix2latex(cbind(A,b)) |> Eqn()

## -----------------------------------------------------------------------------
A <- matrix(paste0("a_{", outer(1:3, 1:3, FUN  = paste0), "}"), 
            nrow=3) |> print()

b <- paste0("b_", 1:3)
x <- paste0("x", 1:3)

## ----showEqn0, eval=FALSE-----------------------------------------------------
#  showEqn(A, b, vars = x, latex=TRUE)

## ----results='asis'-----------------------------------------------------------
showEqn(A, b, vars = x, latex=TRUE) |> Eqn()

