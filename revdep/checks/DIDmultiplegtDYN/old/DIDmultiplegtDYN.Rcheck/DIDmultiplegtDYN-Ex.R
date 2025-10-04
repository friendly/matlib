pkgname <- "DIDmultiplegtDYN"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('DIDmultiplegtDYN')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("did_multiplegt_dyn")
### * did_multiplegt_dyn

flush(stderr()); flush(stdout())

### Name: did_multiplegt_dyn
### Title: Core function for did_multiplegt_dyn
### Aliases: did_multiplegt_dyn

### ** Examples


# See the did_multiplegt_dyn GitHub page for examples and details.




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
