pkgname <- "hyperoverlap"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('hyperoverlap')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("hyperoverlap_detect")
### * hyperoverlap_detect

flush(stderr()); flush(stdout())

### Name: hyperoverlap_detect
### Title: Overlap detection in n-dimensional space using support vector
###   machines (SVMs)
### Aliases: hyperoverlap_detect

### ** Examples


data = iris[which(iris$Species!=("versicolor")),]
x = hyperoverlap_detect(data[,1:3],data$Species, kernel="linear")




cleanEx()
nameEx("hyperoverlap_lda")
### * hyperoverlap_lda

flush(stderr()); flush(stdout())

### Name: hyperoverlap_lda
### Title: Hyperoverlap visualisation using linear discriminant analysis
###   (LDA)
### Aliases: hyperoverlap_lda

### ** Examples

#using iris dataset reduced to two species
data = iris[which(iris$Species!=("versicolor")),]
x = hyperoverlap_detect(data[1:4], data$Species)
hyperoverlap_lda(x)




cleanEx()
nameEx("hyperoverlap_pairs_plot")
### * hyperoverlap_pairs_plot

flush(stderr()); flush(stdout())

### Name: hyperoverlap_pairs_plot
### Title: Overlap heatmap plotting for analysis of multiple entities
### Aliases: hyperoverlap_pairs_plot

### ** Examples

hyperoverlap.iris.set = hyperoverlap_set(iris[1:3],iris$Species, kernel="linear")
hyperoverlap_pairs_plot(hyperoverlap.iris.set)




cleanEx()
nameEx("hyperoverlap_plot")
### * hyperoverlap_plot

flush(stderr()); flush(stdout())

### Name: hyperoverlap_plot
### Title: Overlap plotting for low-dimensional spaces
### Aliases: hyperoverlap_plot

### ** Examples

data = iris[which(iris$Species!=("versicolor")),]
x = hyperoverlap_detect(data[,1:3],data$Species, kernel="linear")
hyperoverlap_plot(x)




cleanEx()
nameEx("hyperoverlap_set")
### * hyperoverlap_set

flush(stderr()); flush(stdout())

### Name: hyperoverlap_set
### Title: Pairwise overlap detection in n-dimensional space of multiple
###   entities using support vector machines (SVMs)
### Aliases: hyperoverlap_set

### ** Examples


data(iris)
hyperoverlap.iris.set = hyperoverlap_set(iris[1:3],iris$Species, kernel="linear")




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
