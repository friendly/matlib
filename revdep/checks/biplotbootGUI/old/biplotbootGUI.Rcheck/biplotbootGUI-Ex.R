pkgname <- "biplotbootGUI"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('biplotbootGUI')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("CDBiplot")
### * CDBiplot

flush(stderr()); flush(stdout())

### Name: CDBiplot
### Title: Clustering and/or Disjoint Biplot
### Aliases: CDBiplot
### Keywords: multivariate

### ** Examples

data(iris)
CDBiplot(iris[,-5], iris[,5])



cleanEx()
nameEx("biplotboot")
### * biplotboot

flush(stderr()); flush(stdout())

### Name: biplotboot
### Title: biplotbootGUI: Bootstrap on Classical Biplots and Clustering
###   Disjoint Biplot
### Aliases: biplotboot
### Keywords: multivariate

### ** Examples

data(iris)
biplotboot(iris[,-5])



cleanEx()
nameEx("biplotbootGUI-package")
### * biplotbootGUI-package

flush(stderr()); flush(stdout())

### Name: biplotbootGUI-package
### Title: biplotbootGUI: Bootstrap on Classical Biplots and Clustering
###   Disjoint Biplot
### Aliases: biplotbootGUI-package biplotbootGUI
### Keywords: package

### ** Examples

data(iris)
biplotboot(iris[,-5])


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
