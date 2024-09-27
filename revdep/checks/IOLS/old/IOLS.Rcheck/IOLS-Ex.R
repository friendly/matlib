pkgname <- "IOLS"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('IOLS')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("iOLS")
### * iOLS

flush(stderr()); flush(stdout())

### Name: iOLS
### Title: iOLS
### Aliases: iOLS

### ** Examples

data(DATASET)
y = DATASET$y
x = as.matrix(DATASET[,c("X1","X2")])
lm = lm(log(y+1) ~ x)
lm_coef = c(coef(lm))
X = cbind(rep(1, nrow(x)), x)
tX = t(X)
library(matlib) ; VX = inv(tX %*% X)
f = iOLS(y, X, VX, tX, 20, b_init = lm_coef)




cleanEx()
nameEx("iOLS_path")
### * iOLS_path

flush(stderr()); flush(stdout())

### Name: iOLS_path
### Title: iOLS_path
### Aliases: iOLS_path

### ** Examples

data(DATASET)
y = DATASET$y
x = as.matrix(DATASET[,c("X1","X2")])
lm = lm(log(y+1) ~ x)
lm_coef = c(coef(lm))
X = cbind(rep(1, nrow(x)), x)
k = iOLS_path(y, X, b_init = lm_coef,
deltainf = 10^-5, deltasup = 10^4, nbre_delta = 20,
epsi = 10^-3, error_type = "HC0")




cleanEx()
nameEx("iOLS_path_plot")
### * iOLS_path_plot

flush(stderr()); flush(stdout())

### Name: iOLS_path_plot
### Title: iOLS_path_plot
### Aliases: iOLS_path_plot

### ** Examples

data(DATASET)
y = DATASET$y
x = as.matrix(DATASET[,c("X1","X2")])
lm = lm(log(y+1) ~ x)
lm_coef = c(coef(lm))
X = cbind(rep(1, nrow(x)), x)
k = iOLS_path(y, X, b_init = lm_coef, deltainf = 10^-5,
deltasup = 10^4, nbre_delta = 20,
epsi = 10^-3, error_type = "HC0")

#All the parameters, as a function of log(delta) (ie. each triplet from an iOLS regression) :
iOLS_path_plot(k)

#All the parameters from the 6th iOLS regression :
iOLS_path_plot(k, delta_rank = 6)

#Intercept from the 6th iOLS regression :
iOLS_path_plot(k, delta_rank = 6, plot_beta = 0)




cleanEx()
nameEx("iOLS_plot")
### * iOLS_plot

flush(stderr()); flush(stdout())

### Name: iOLS_plot
### Title: iOLS_plot
### Aliases: iOLS_plot

### ** Examples

data(DATASET)
y = DATASET$y
x = as.matrix(DATASET[,c("X1","X2")])
lm = lm(log(y+1) ~ x)
lm_coef = c(coef(lm))
X = cbind(rep(1, nrow(x)), x)
tX = t(X)
library(matlib) ; VX = inv(tX %*% X)
f = iOLS(y, X, VX, tX, 20, b_init = lm_coef)

iOLS_plot(f)

#Only one of the estimated parameters, for example k=0 (the intercept):
iOLS_plot(f, plot_beta = 0)




cleanEx()
nameEx("lambda_test")
### * lambda_test

flush(stderr()); flush(stdout())

### Name: lambda_test
### Title: lambda_test
### Aliases: lambda_test

### ** Examples

data(DATASET)
y = DATASET$y
x = as.matrix(DATASET[,c("X1","X2")])
lm = lm(log(y+1) ~ x)
lm_coef = c(coef(lm))
X = cbind(rep(1, nrow(x)), x)
k = iOLS_path(y, X, b_init = lm_coef, deltainf = 10^-5,
deltasup = 10^4, nbre_delta = 20,
epsi = 10^-3, error_type = "HC0")

L = lambda_test(k, nB = 5)




cleanEx()
nameEx("print")
### * print

flush(stderr()); flush(stdout())

### Name: print
### Title: print.iOLS
### Aliases: print

### ** Examples

data(DATASET)
y = DATASET$y
x = as.matrix(DATASET[,c("X1","X2")])
lm = lm(log(y+1) ~ x)
lm_coef = c(coef(lm))
X = cbind(rep(1, nrow(x)), x)
tX = t(X)
library(matlib) ; VX = inv(tX %*% X)
f = iOLS(y, X, VX, tX, 20, b_init = lm_coef)
print(f)




cleanEx()
nameEx("print.iOLS_path")
### * print.iOLS_path

flush(stderr()); flush(stdout())

### Name: print.iOLS_path
### Title: print.iOLS_path
### Aliases: print.iOLS_path

### ** Examples

data(DATASET)
y = DATASET$y
x = as.matrix(DATASET[,c("X1","X2")])
lm = lm(log(y+1) ~ x)
lm_coef = c(coef(lm))
X = cbind(rep(1, nrow(x)), x)
k = iOLS_path(y, X, b_init = lm_coef, deltainf = 10^-5,
deltasup = 10^4, nbre_delta = 20,
epsi = 10^-3, error_type = "HC0")

#Printing of all the iOLS regression:
print(k)

#Printing of the 6th iOLS regression :
print(k, delta_rank = 6)




cleanEx()
nameEx("print.lambda_test")
### * print.lambda_test

flush(stderr()); flush(stdout())

### Name: print.lambda_test
### Title: print.lambda_test
### Aliases: print.lambda_test

### ** Examples

data(DATASET)
y = DATASET$y
x = as.matrix(DATASET[,c("X1","X2")])
lm = lm(log(y+1) ~ x)
lm_coef = c(coef(lm))
X = cbind(rep(1, nrow(x)), x)
k = iOLS_path(y, X, b_init = lm_coef, deltainf = 10^-5,
deltasup = 10^4, nbre_delta = 20,
epsi = 10^-3, error_type = "HC0")

L = lambda_test(k, nB = 5)

print(L)



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
