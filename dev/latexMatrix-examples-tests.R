library(matlib)

latexMatrix()
latexMatrix(prefix="\\sqrt{", suffix="}")
latexMatrix("\\beta")
latexMatrix("\\beta", prefix="\\sqrt{", suffix="}")
latexMatrix(nrow="n", ncol="n")
latexMatrix(nrow="n", ncol="n", prefix="\\sqrt{", suffix="}")
latexMatrix(ncol=3)
latexMatrix(ncol=3, prefix="\\sqrt{", suffix="}")
latexMatrix(nrow=4)
latexMatrix(nrow=4, prefix="\\sqrt{", suffix="}")
latexMatrix(nrow=4, ncol=4)
latexMatrix(nrow=4, ncol=4,  prefix="\\sqrt{", suffix="}")
latexMatrix(nrow=1)
latexMatrix(nrow=1, prefix="\\sqrt{", suffix="}") 
latexMatrix(ncol=1)
latexMatrix(ncol=1,  prefix="\\sqrt{", suffix="}")
latexMatrix(nrow=1, ncol=4)
latexMatrix(nrow=1, ncol=4, prefix="\\sqrt{", suffix="}")
latexMatrix(nrow=3, ncol=1)
latexMatrix(nrow=3, ncol=1, prefix="\\sqrt{", suffix="}")
latexMatrix(nrow=3, ncol=3, diag=TRUE)
latexMatrix(nrow=3, ncol=3, diag=TRUE, prefix="\\sqrt{", suffix="}") 
latexMatrix(nrow="n", ncol="n", diag=TRUE)
latexMatrix(nrow="n", ncol="n", diag=TRUE, prefix="\\sqrt{", suffix="}") 

latexMatrix(comma="TRUE")
latexMatrix(comma="TRUE", prefix="\\sqrt{", suffix="}")
latexMatrix("\\beta", comma=TRUE)
latexMatrix(nrow="n", ncol="n", comma=TRUE)
latexMatrix(ncol=3, comma=TRUE)
latexMatrix(nrow=4, comma=TRUE)
latexMatrix(nrow=4, ncol=4, comma=TRUE)
latexMatrix("\\beta", nrow="k", ncol="k", comma=TRUE, exponent="-1")
latexMatrix("\\beta", comma=TRUE, transpose=TRUE)
latexMatrix("\\beta", nrow="k", ncol="k", comma=TRUE, exponent="-1", transpose=TRUE)
latexMatrix(nrow=1, comma=TRUE)
latexMatrix(ncol=1, comma=TRUE)
latexMatrix(nrow=1, ncol=4, comma=TRUE)
latexMatrix(nrow=3, ncol=1, comma=TRUE)
latexMatrix(nrow=3, ncol=3, diag=TRUE, comma=TRUE)
latexMatrix(nrow="n", ncol="n", diag=TRUE, comma=TRUE)
latexMatrix(nrow=3.1, ncol=3) # error (expected)

latexMatrix("\\lambda", nrow="n", ncol="n", diag=TRUE, lhs="\\boldsymbol{\\Lambda}")

X <- latexMatrix("x", "n", "p")
U <- latexMatrix("u", "n", "k")
D <- latexMatrix("\\lambda", "k", "k", diag=TRUE)
V <- latexMatrix("v", "k", "p", transpose = TRUE)
cat("SVD:\n", getLatex(X), "=\n", getLatex(U), 
    "\n", getLatex(D), "\n", getLatex(V))

m <- matrix(c(
  "\\alpha", "\\beta",
  "\\gamma", "\\delta",
  "\\epsilon", "\\pi",
  0 , 0), 4, 2, byrow=TRUE)
latexMatrix(m)
latexMatrix(m, transpose=TRUE)
latexMatrix(m, exponent="-1")
latexMatrix(m, transpose=TRUE, exponent="-1")

latexMatrix(m, prefix="\\sqrt{", suffix="}")
latexMatrix(m, prefix="{", suffix="}^{1/2}")


latexMatrix(diag(4))

latexMatrix()
latexMatrix(prefix="\\sqrt{", suffix="}")
latexMatrix(prefix="{", suffix="^{1/2}")

latexMatrix(nrow=4)
latexMatrix(nrow=4, prefix="\\sqrt{", suffix="}")
latexMatrix(prefix="{", suffix="^{1/2}")

latexMatrix(1:4) # error (expected)
latexMatrix(LETTERS[1:4]) # error (expected)

latexMatrix(show.size=TRUE) 
latexMatrix(nrow=3, show.size=TRUE) 
latexMatrix(ncol=3, show.size=TRUE) 
latexMatrix(nrow=3, ncol=3, show.size=TRUE) 
latexMatrix(show.size=TRUE, transpose=TRUE) 
latexMatrix(show.size=TRUE, transpose=TRUE, exponent="-1") 

m <- matrix(3/(1:9), 3, 3)
latexMatrix(m)
latexMatrix(m, digits=2)
latexMatrix(m, fractions=TRUE)
m[1, 1] <- - m[1, 1] 
m[2, 2] <- - m[2, 2] 
m[3, 3] <- - m[3, 3] 
latexMatrix(m, fractions=TRUE)

latexMatrix(zero.based=c(TRUE, TRUE))
latexMatrix(nrow=3, ncol=4, zero.based=c(TRUE, TRUE))  # error (expected)
latexMatrix(ncol=4, zero.based=c(TRUE, FALSE)) 
latexMatrix(nrow=3, zero.based=c(FALSE, TRUE)) 
latexMatrix(nrow="n", ncol="n", diag=TRUE, zero.based=c(TRUE, TRUE))

latexMatrix(zero.based=c(TRUE, FALSE))
latexMatrix(zero.based=c(FALSE, TRUE))

latexMatrix(nrow=3, ncol="m", zero.based=c(FALSE, TRUE),
               end.at=c("n-1", "m")) 

latexMatrix("\\mathbf{B}", nrow="q", ncol="p", 
               zero.based=c(TRUE, FALSE), 
               end.at=c("n-1", "m"))

latexMatrix(zero.based=c(TRUE, TRUE), end.at=c("n - 1", "m - 1"))
latexMatrix(zero.based=c(TRUE, TRUE), end.at=c("n", "m"))
latexMatrix(zero.based=c(TRUE, TRUE), end.at=c("n - 1", "m"))
latexMatrix(zero.based=c(TRUE, TRUE), end.at=c("n", "m - 1"))
latexMatrix(zero.based=c(TRUE, FALSE), end.at=c("n - 1", "m - 1"))

latexMatrix(zero.based=c(TRUE, TRUE), end.at=c('n - 1', "m - 1"),
               show.size=TRUE)
latexMatrix(zero.based=c(TRUE, TRUE), end.at=c("n", "m"),
               show.size=TRUE)
latexMatrix(zero.based=c(TRUE, TRUE), end.at=c("n - 1", "m"),
               show.size=TRUE)
latexMatrix(zero.based=c(TRUE, TRUE), end.at=c("n", "m-1"),
               show.size=TRUE)
latexMatrix(zero.based=c(TRUE, FALSE), end.at=c("n - 1", "m - 1"),
               show.size=TRUE)

matrix(c(1,3,0,1),2,2) |> latexMatrix(matrix="bmatrix") -> A
matrix(c(5,3,1,4),2,2) |> latexMatrix(matrix="bmatrix") -> B
C <- latexMatrix(nrow=2, ncol=2)
D <- latexMatrix()
A
B
C
D
Nrow(D)
Ncol(D)
Dim(D)
getBody(A)
getWrapper(A)
getLatex(A)
Dim(A)

A + B
A + C

# extractors
getBody(A + B)
getWrapper(A + B)
getLatex(A + B)

getLatex(A + B) |> cat()

cat(getLatex(A), " +\\large\n", getLatex(B), "\\quad\\large=\\quad\n", getLatex(A + B))

# or keeping the numeric version
A <-matrix(c(1,3,0,1),2,2)
getLatex(latexMatrix(A))
B <- matrix(c(5,3,1,4),2,2)
getLatex(latexMatrix(B))
getLatex(latexMatrix(A + B))



Eqn("\\mathbf{A} + \\mathbf{B} =", A, " + ", B, " = ", A + B)
Eqn(A, " + ", D, " = ", A + D) # error (expected, message could be better)
Eqn(A, " + ", B, " = ", A + "foo")

Z <- latexMatrix(matrix(1:6, 3, 2))
Z
t(Z)

M <- latexMatrix(matrix="bmatrix")
M
t(M)   # error (expected)
Dim(M)

X <- latexMatrix(matrix(letters[1:6], 2, 3))
Y <- latexMatrix(matrix(LETTERS[1:6], 3, 2))
X
Y
X %*% Y

W <- latexMatrix(matrix(letters[7:12], 2, 3))
X + W
(X + W) %*% Y

X <- latexMatrix(matrix(1:6, 2, 3), matrix="bmatrix")
Y <- latexMatrix(matrix(10*(1:6), 3, 2), matrix="bmatrix")
X
t(Y)
X %*% Y

# element-wise multiplication
tY <- t(Y)
X * tY # error (expected)

a <- latexMatrix('a', nrow=1, ncol=1)
a * X # error (expected)

A <- latexMatrix(matrix(letters[1:4], 2, 2, byrow=TRUE))
determinant(A)

B <- latexMatrix(matrix(letters[1:9], 3, 3, byrow=TRUE))
determinant(B)

(C <- latexMatrix(matrix(1:9, nrow=3, ncol=3)))
(D <- latexMatrix(matrix(11:19, nrow=3, ncol=3)))
as.numeric(C)
as.numeric(D)
(E <- C + D)
as.numeric(E)
(F <- C %*% D)
as.numeric(F)

(G <- latexMatrix(matrix(letters[1:4], 2, 2)))
as.numeric(G) # error (expected)
as.numeric(G, locals=list(a=1, b=2, c=3, d=4))

A <- latexMatrix(nrow=2, ncol=3)
B <- latexMatrix(nrow=2, ncol=3)
A + B

C <- latexMatrix()
D <- latexMatrix()
C + D # error (expected)
A <- latexMatrix(matrix(letters[1:9], 3, 3, byrow=TRUE))
A
solve(A)
Eqn(solve(A, simplify=TRUE))

# check again Wolfram Alpha: www.wolframalpha.com
# inverse of {{a, b, c}, {d, e, f}, {g, h, i}}

B <- latexMatrix(matrix(letters[1:4], 2, 2, byrow=TRUE),
                    matrix="\\bmatrix")
B
solve(B)
Eqn(solve(B, simplify=TRUE))

# example from <https://www.vedantu.com/maths/inverse-of-a-matrix-using-minors-cofactors-and-adjugate>
X <- latexMatrix(matrix(c(3,2,0,1,1,1,2,-2,1), 3, 3))
X
solve(X)
MASS::fractions(as.numeric(solve(X)))
MASS::fractions(solve(as.numeric(X))) # check
MASS::fractions(as.numeric(solve(A),
                           locals=list(a=3, d=2, g=0,
                                       b=1, e=1, h=1,
                                       c=2, f=-2, i=1)))

W <- latexMatrix(nrow=2, ncol=3, matrix="\\bmatrix")
W
1*W
W*"a"
W*W # error (expected)
letters*W # error (expected)
"a"*(W + W)
-(W + W)


# linear hypotheses

Eqn("\\mathcal{H}_0 : \\mathbf{C} \\mathbf{B} & = ",
    latexMatrix(matrix(c(0,1,1,0), nrow=1)),
    latexMatrix('\\beta', ncol = 3, nrow=4, comma=TRUE, prefix.col = 'y_'),
    Eqn_newline(), Eqn_newline(), 
    latexMatrix('\\beta', ncol = 3, nrow=2, comma=TRUE, prefix.col = 'y_', lhs = '&' ),
    "= \\mathbf{0}_{(2 \\times 3)}", 
    align=TRUE)
