pkgname <- "MPkn"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('MPkn')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("MPKlMatrix")
### * MPKlMatrix

flush(stderr()); flush(stdout())

### Name: MPKlMatrix
### Title: Creates a matrix of specified row of output 'MUPkLo'
### Aliases: MPKlMatrix
### Keywords: MPKlMatrix

### ** Examples

A <- array(c(0.9, 0.6, 0.8, 0.05, 0.2, 0.05, 0.05, 0.2, 0.15), c(3, 3))
P <- array(c(0.9, 0.6, 0.8, 0.05, 0.2, 0.05, 0.05, 0.2, 0.15), c(3, 3))
U <- array(c(0.8, 0.8, 0.7, 0.06, 0.02, 0.2, 0.14, 0.18, 0.1), c(3, 3))
sta <- c(1, 2, 3)
k <- c(1, 0, 1, 0)
n <- c(5, 7, 12, 17)
Mx <- MUPkLo(A, P, U, n, k, sta)
M100 = MPKlMatrix(Mx, step = 100, nc = 3, sta = c(1, 2, 3))



cleanEx()
nameEx("MPkn-package")
### * MPkn-package

flush(stderr()); flush(stdout())

### Name: MPkn-package
### Title: Calculations of One Discrete Model in Several Time Steps
### Aliases: MPkn-package

### ** Examples

require(MPkn)
require(markovchain)
options(digits = 14)
n = 12
k = 2
rz = 11
P = array(0, c(rz, rz))
for (i in 1:rz){
  po = runif(rz)
  P[i, ] = po/sum(po)
}
I = diag(1, rz, rz)
Myy = MUPkL(P, P, I, n, k, c(1:rz))
StSy = NULL
for (i in 1:rz) StSy = c(StSy, Myy$Navg[,,i][n])
mrkv = new("markovchain", transitionMatrix = P)
StSx = steadyStates(mrkv)
print("MPkn");  print(StSy)
print("markovchain"); print(StSx)




cleanEx()
nameEx("MUPkL")
### * MUPkL

flush(stderr()); flush(stdout())

### Name: MUPkL
### Title: Calculations of one discrete model in several time steps
### Aliases: MUPkL
### Keywords: MUPkL

### ** Examples

A <- array(c(2, 3, 1, 4, 2, 1, 3, 1, 2), c(3, 3))
P <- array(c(0.9, 0.6, 0.8, 0.05, 0.2, 0.05, 0.05, 0.2, 0.15),
					 c(3, 3))
U <- array(c(0.8, 0.8, 0.7, 0.06, 0.02, 0.2, 0.14, 0.18, 0.1),
					 c(3, 3))
sta <- c(1, 3)
k <- 3
n <- 8
M33 <- MUPkL(A, P, U, n, k, sta)
print(M33$N)
k <- 1
n <- 24
M11 <- MUPkL(A, P, U, n, k, sta)
print(M11$N)
k <- 0
n <- 6
M00 <- MUPkL(A, P, U, n, k, sta)
print(M00$N)



cleanEx()
nameEx("MUPkLo")
### * MUPkLo

flush(stderr()); flush(stdout())

### Name: MUPkLo
### Title: Calculations of one discrete model in several time steps
### Aliases: MUPkLo
### Keywords: MUPkLo

### ** Examples

A = array(c(-2, -3, 1, 4, -2, 1, 3, -1, -2), c(3, 3))
P <- array(c(0.9, 0.6, 0.8, 0.05, 0.2, 0.05, 0.05, 0.2, 0.15), c(3, 3))
U <- array(c(0.8, 0.8, 0.7, 0.06, 0.02, 0.2, 0.14, 0.18, 0.1), c(3, 3))
sta <- 3
Ao <- A
k <- c(1, 0, 1, 0)
n <- c(5, 7, 12, 17)
# Steps, in which will compute the value of the Mx:
# 1, 2, 3, 4, 5, 10, 20, 21,  22, 23, 24, 25, 50, 100, 200, 400, 800
Mx <- MUPkLo(A, P, U, n, k, sta)
print(Mx)
A <- Ao
Mb <- MUPkLo(A, P, U, n = 100, k = 1, sta)
Mb[100,,]



cleanEx()
nameEx("matrix.powerni")
### * matrix.powerni

flush(stderr()); flush(stdout())

### Name: matrix.powerni
### Title: Matrix Power of Non Integer
### Aliases: matrix.powerni
### Keywords: matrix.powerni

### ** Examples

require(MPkn)
require(matrixcalc)
matmult <- function(A, B){
	C = matrix(numeric(4), 2, 2)
	for (i in 1:2){
		for (j in 1:2){ C[i, j] = sum(A[i, ]*B[, j])}
	}
	return(C)
}
I = diag(1, 2, 2)
P = matrix(c(0.9, 0.3, 0.1, 0.7), 2, 2)
M1 = P
M2 = matmult((I + P), M1)
M4 = matmult((I + t(matrix.power(P, 2))), M2)
M8 = matmult((I + t(matrix.power(P, 4))), M4)
M16 = matmult((I + t(matrix.power(P, 8))), M8)
## =====================
Q = list()
Q[[1]] = M1
Q[[2]] = matmult(M2, matrix.inverse(M1)) - I
Q[[3]] = matrix.powerni(matmult(M4, matrix.inverse(M2)) - I, 1/2)
Q[[4]] = matrix.powerni(matmult(M8, matrix.inverse(M4)) - I, 1/4)
Q[[5]] = matrix.powerni(matmult(M16, matrix.inverse(M8)) - I, 1/8)
print("Q"); print(Q)
S = as.matrix(Q[[1]], 2, 2)
for (i in 2:5){
  S = S + as.matrix(Q[[i]], 2, 2)
}
Qs = S/5
print("Qs"); print(Qs)



cleanEx()
nameEx("radekW")
### * radekW

flush(stderr()); flush(stdout())

### Name: radekW
### Title: The Numbers of Rows of the Output Matrix
### Aliases: radekW
### Keywords: radekW

### ** Examples

  radekW(n = c(3, 5, 8, 9, 11), k = c(1, 0, 1, 0, 0))



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
