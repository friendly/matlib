# test symbolicMatrix(), Eqn()

# John's dev version
source(here::here("dev", "symbolicMatrix.R"))
source(here::here("dev", "Arith.R"))

source(here::here("R","Eqn.R"))

symbolicMatrix()
# return value
mat <- symbolicMatrix(print = FALSE)
str(mat)
cat(mat)
# copy to clipboard
clipr::write_clip(mat)

# use a complex symbol
symbolicMatrix("\\widehat{\\beta}", 2, 4)

symbolicMatrix("\\beta")
symbolicMatrix(nrow="n", ncol="n")
symbolicMatrix(ncol=3)
symbolicMatrix(nrow=4)
symbolicMatrix(nrow=4, ncol=4)
# vectors
symbolicMatrix(nrow=1)
symbolicMatrix(ncol=1)
symbolicMatrix(nrow=1, ncol=4)
symbolicMatrix(nrow=3, ncol=1)

# diagonal matrices
symbolicMatrix(nrow=3, ncol=3, diag=TRUE)
symbolicMatrix(nrow="n", ncol="n", diag=TRUE)

L <- symbolicMatrix("\\lambda", nrow=3, ncol=3, diag=TRUE)
L <- paste0("\\boldsymbol{\\Lambda} =\n", L)
# or now:
symbolicMatrix("\\lambda", nrow=3, ncol=3, 
               diag=TRUE, lhs = "\\boldsymbol{\\Lambda}")

# John's new features
symbolicMatrix(comma="TRUE")
symbolicMatrix("\\beta", comma=TRUE)
symbolicMatrix(nrow="n", ncol="n", comma=TRUE)
symbolicMatrix(ncol=3, comma=TRUE)
symbolicMatrix(nrow=4, comma=TRUE)
symbolicMatrix(nrow=4, ncol=4, comma=TRUE)
symbolicMatrix("\\beta", comma=TRUE, exponent="-1")
symbolicMatrix("\\beta", comma=TRUE, transpose=TRUE)
symbolicMatrix("\\beta", comma=TRUE, exponent="-1", transpose=TRUE)
symbolicMatrix(nrow=1, comma=TRUE)
symbolicMatrix(ncol=1, comma=TRUE)
symbolicMatrix(nrow=1, ncol=4, comma=TRUE)
symbolicMatrix(nrow=3, ncol=1, comma=TRUE)
symbolicMatrix(nrow=3, ncol=3, diag=TRUE, comma=TRUE)
symbolicMatrix(nrow="n", ncol="n", diag=TRUE, comma=TRUE)
symbolicMatrix(nrow=3.1, ncol=3)

# SVD
# this no longer works:

X <- symbolicMatrix("x", "n", "p")
U <- symbolicMatrix("u", "n", "k")
D <- symbolicMatrix("\\lambda", "k", "k", diag=TRUE)
V <- symbolicMatrix("v", "k", "p", transpose = TRUE)
cat("\\mathrm{SVD:}\n", X, "=\n", U, D, V)

#cat("SVD:\n", X, "=\n", U, "\n", D, "\n", V)
# Error in cat("\\mathrm{SVD:}\n", X, "=\n", U, D, V) : 
#   argument 2 (type 'list') cannot be handled by 'cat'


# John's new version

m <- matrix(c(
   "\\alpha", "\\beta",
   "\\gamma", "\\delta",
   "\\epsilon", "\\pi",
   0 , 0), 4, 2, byrow=TRUE)

symbolicMatrix(m)

symbolicMatrix(diag(4), lhs = "\\mathbf{I}_4")

symbolicMatrix(m, transpose=TRUE)
symbolicMatrix(m, exponent="-1")
symbolicMatrix(m, transpose=TRUE, exponent="-1")

# prefix / suffix
symbolicMatrix(prefix="\\sqrt{", suffix="}")

# doesn't compile
# symbolicMatrix(prefix="{", suffix="^{1/2}")

# OK
symbolicMatrix(prefix="", suffix="^{1/2}")


# numerics that aren't integers: options(digits) has no effect
mm <- matrix(1:4, 2,2)
symbolicMatrix(sqrt(mm))

# what about a vector? -- now trapped & documented
symbolicMatrix(1:4)
# Error in symbolicMatrix(1:4) : 
#   'symbol' must be a single character string or a matrix. Hint: wrap a vector in matrix(), with nrow=1 or ncol=1.
# symbolicMatrix(matrix(letters[1:4], nrow=1))

# testing zero.based

ymbolicMatrix(zero.based=TRUE)
symbolicMatrix(nrow=3, ncol=4, zero.based=TRUE)
symbolicMatrix(ncol=4, zero.based=TRUE)
symbolicMatrix(nrow=3, zero.based=TRUE)
symbolicMatrix(nrow="n", ncol="n", diag=TRUE, zero.based=TRUE)
symbolicMatrix(nrow=3, ncol=3, diag=TRUE, zero.based=TRUE)

symbolicMatrix(zero.based=c(TRUE, FALSE))
symbolicMatrix(zero.based=c(FALSE, TRUE))


symbolicMatrix("\\mathbf{B}", nrow="q", ncol="p",
               zero.based=c(TRUE, FALSE))

symbolicMatrix("\\mathbf{B}", nrow="q", ncol="p",
               zero.based=c(TRUE, FALSE),
               end.at.n.minus.1=c(FALSE, TRUE))



# using R/Eqn()


Eqn({
    cat("X&=U \\lambda V \\\\ \n")
    symbolicMatrix("u", "n", "k", lhs = '&')
    symbolicMatrix("\\lambda", "k", "k", diag=TRUE)
    symbolicMatrix("v", "k", "p", transpose = TRUE)
   }, align=TRUE)


# testing Arith.R

source(here::here("dev","Arith.R"))

matrix(c(1,3,0,1),2,2) |> symbolicMatrix(matrix="bmatrix") -> A
matrix(c(5,3,1,4),2,2) |> symbolicMatrix(matrix="bmatrix") -> B
C <- symbolicMatrix(nrow=2, ncol=2)
D <- symbolicMatrix()
A
B
C
D

getBody(A)
getWrapper(A)
getMatrix(A)

A + B
A + C

# extractors
getBody(A + B)
getWrapper(A + B)
getMatrix(A + B)

getMatrix(A + B) |> cat()

# SVD using new symbolicMatrix extractors

X <- symbolicMatrix("x", "n", "p")
U <- symbolicMatrix("u", "n", "k")
D <- symbolicMatrix("\\lambda", "k", "k", diag=TRUE)
V <- symbolicMatrix("v", "k", "p", transpose = TRUE)
cat("SVD:\n", getMatrix(X), "=\n", getMatrix(U),
        "\n", getMatrix(D), "\n", getMatrix(V))

