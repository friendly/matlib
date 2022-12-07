pkgname <- "IOLS"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('IOLS')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
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
nameEx("print")
### * print

flush(stderr()); flush(stdout())

### Name: print
### Title: print.lambda_test
### Aliases: print

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
