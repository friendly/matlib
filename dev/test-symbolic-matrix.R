# test symbolicMatrix

source(here::here("dev", "symbolicMatrix.R"))

symbolicMatrix()
# return value
mat <- symbolicMatrix(print = FALSE)
str(mat)
cat(mat)
# copy to clipboard
clipr::write_clip(mat)

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

L <- symbolicMatrix("\lambda", nrow=3, ncol=3, diag=TRUE)
L <- paste0("\\boldsymbol{\\Lambda} =\n", L)
# or now:
symbolicMatrix("\\lambda", nrow=3, ncol=3, diag=TRUE, lhs = "\\boldsymbol{\\Lambda}")

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

X <- symbolicMatrix("x", "n", "p", print=FALSE)
U <- symbolicMatrix("u", "n", "k", print=FALSE)
D <- symbolicMatrix("\\lambda", "k", "k", diag=TRUE, print=FALSE)
V <- symbolicMatrix("v", "k", "p", transpose = TRUE, print=FALSE)
cat("\\mathrm{SVD:}\n", X, "=\n", U, D, V)

#cat("SVD:\n", X, "=\n", U, "\n", D, "\n", V)

