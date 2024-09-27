pkgname <- "VIRF"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('VIRF')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("VIRF")
### * VIRF

flush(stderr()); flush(stdout())

### Name: VIRF
### Title: Volatility Impulse Response Function
### Aliases: VIRF

### ** Examples



k=3 #number of series
p=6 # maximum lag order
ns=100 #number of simulations
B=matrix(0,nrow=k,ncol=p*k)
A1<- matrix(c(.4,-.02,.01,-.02,.3,.02,.01,.04,.3),ncol=3,nrow=3)
A2 <- matrix(c(.2,0,0,0,.3,0,0,0,.13),ncol=3,nrow=3)
B[,1:k]=A1
B[,(4*k+1):(5*k)]=A2
A <- BigVAR::VarptoVar1MC(B,p,k)
Y <-BigVAR::MultVarSim(k,A,p,.1*diag(k),ns)
lr<-VIRF(Y,40) # Y: multivariate time series data, shock time point: 40
print(lr)




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
