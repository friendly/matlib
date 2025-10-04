pkgname <- "MVSKmod"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('MVSKmod')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("MVNIGmod")
### * MVNIGmod

flush(stderr()); flush(stdout())

### Name: MVNIGmod
### Title: AECM Estimation for Matrix-Variate Normal-Inverse Gaussian
###   Models
### Aliases: MVNIGmod

### ** Examples


MVNIGmod(Y,X,theta_mvnig)





cleanEx()
nameEx("MVVGmod")
### * MVVGmod

flush(stderr()); flush(stdout())

### Name: MVVGmod
### Title: AECM Estimation for Matrix-Variate Variance Gamma (MVVG) Models
### Aliases: MVVGmod

### ** Examples

MVVGmod(Y,X,theta_mvvg)





cleanEx()
nameEx("X")
### * X

flush(stderr()); flush(stdout())

### Name: X
### Title: Toy Covariate Matrices
### Aliases: X
### Keywords: datasets

### ** Examples

X



cleanEx()
nameEx("Y")
### * Y

flush(stderr()); flush(stdout())

### Name: Y
### Title: Toy Response Matrices
### Aliases: Y
### Keywords: datasets

### ** Examples

Y



cleanEx()
nameEx("gaad_cov")
### * gaad_cov

flush(stderr()); flush(stdout())

### Name: gaad_cov
### Title: GAAD Data
### Aliases: gaad_cov
### Keywords: datasets

### ** Examples

gaad_cov
gaad_res



cleanEx()
nameEx("gaad_res")
### * gaad_res

flush(stderr()); flush(stdout())

### Name: gaad_res
### Title: GAAD Data
### Aliases: gaad_res
### Keywords: datasets

### ** Examples

gaad_cov
gaad_res



cleanEx()
nameEx("gaad_theta_mvvg")
### * gaad_theta_mvvg

flush(stderr()); flush(stdout())

### Name: gaad_theta_mvvg
### Title: MVVG Parameter Format
### Aliases: gaad_theta_mvvg
### Keywords: datasets

### ** Examples

gaad_theta_mvvg



cleanEx()
nameEx("predict")
### * predict

flush(stderr()); flush(stdout())

### Name: predict
### Title: MVSK Model Prediction
### Aliases: predict

### ** Examples




cleanEx()
nameEx("theta_mvnig")
### * theta_mvnig

flush(stderr()); flush(stdout())

### Name: theta_mvnig
### Title: Toy Response Initial Parameter (MVNIG)
### Aliases: theta_mvnig
### Keywords: datasets

### ** Examples

theta_mvvg



cleanEx()
nameEx("theta_mvvg")
### * theta_mvvg

flush(stderr()); flush(stdout())

### Name: theta_mvvg
### Title: Toy Response Initial Parameter (MVVG)
### Aliases: theta_mvvg
### Keywords: datasets

### ** Examples

theta_mvvg



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
