pkgname <- "smallstuff"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('smallstuff')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("CI")
### * CI

flush(stderr()); flush(stdout())

### Name: CI
### Title: Normal Confidence Interval
### Aliases: CI

### ** Examples

CI()
CI(150,5,30,.9)



cleanEx()
nameEx("CVerror")
### * CVerror

flush(stderr()); flush(stdout())

### Name: CVerror
### Title: k-Fold Cross Validation Error Rate
### Aliases: CVerror

### ** Examples

mtcars$am=as.factor(mtcars$am)
gmod=glm(am~mpg,binomial,mtcars)
CVerror(gmod)



cleanEx()
nameEx("CVerrorknn")
### * CVerrorknn

flush(stderr()); flush(stdout())

### Name: CVerrorknn
### Title: k-Fold Cross Validation Error Rate for KNN
### Aliases: CVerrorknn

### ** Examples

mtcars$am=as.factor(mtcars$am)
CVerrorknn(mtcars[,c("mpg","hp")],mtcars$am)



cleanEx()
nameEx("ROCcurve")
### * ROCcurve

flush(stderr()); flush(stdout())

### Name: ROCcurve
### Title: Plot the ROC curve
### Aliases: ROCcurve

### ** Examples

gmod=glm(state~.,binomial,Puromycin)
ROCcurve(gmod)



cleanEx()
nameEx("ROCknn")
### * ROCknn

flush(stderr()); flush(stdout())

### Name: ROCknn
### Title: KNN ROC curve
### Aliases: ROCknn

### ** Examples

yhat=class::knn(Puromycin[,c("conc","rate")],Puromycin[,c("conc","rate")],
         Puromycin$state,10,prob=TRUE)
ROCknn(yhat,Puromycin$state)



cleanEx()
nameEx("allspan3D")
### * allspan3D

flush(stderr()); flush(stdout())

### Name: allspan3D
### Title: Plot Span and Vectors in 3D
### Aliases: allspan3D

### ** Examples

M=matrix(c(1,2,4,3,0,2),3)
oldpar <- par(mfrow=c(3,2))
allspan3D(M,cbind(M,M[,1]-M[,2]),V2=matrix(c(rep(0,6),M[,2]),3),col=c(2,2,1))
par(oldpar)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("allvectors3D")
### * allvectors3D

flush(stderr()); flush(stdout())

### Name: allvectors3D
### Title: Plot Vectors in 3D
### Aliases: allvectors3D

### ** Examples

a=c(2,4,8)
b=c(6,0,4)
oldpar <- par(mfrow=c(3,2))
allvectors3D(cbind(a,b,a-b),V2=matrix(c(rep(0,6),b),3))
par(oldpar)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("as_adj_def")
### * as_adj_def

flush(stderr()); flush(stdout())

### Name: as_adj_def
### Title: Create an adjacency matrix from a multigraph according to the
###   definition
### Aliases: as_adj_def

### ** Examples

g=igraph::graph_from_literal(1-2,2-2:3:3:4,3-4:5:6,5-1:1:1,6-6,simplify=FALSE)
as_adj_def(g)



cleanEx()
nameEx("coord2D")
### * coord2D

flush(stderr()); flush(stdout())

### Name: coord2D
### Title: Plot a 2D Coordinate System
### Aliases: coord2D

### ** Examples

coord2D()



cleanEx()
nameEx("coord3D")
### * coord3D

flush(stderr()); flush(stdout())

### Name: coord3D
### Title: Plot a 3D Coordinate System
### Aliases: coord3D

### ** Examples

coord3D()



cleanEx()
nameEx("crossing2")
### * crossing2

flush(stderr()); flush(stdout())

### Name: crossing2
### Title: Find Edge Crossings
### Aliases: crossing2

### ** Examples

g=igraph::graph_from_literal(1-2,2-3:4,3-4:5:6,5-1)
split=c("A","A","B","B","A","B")
igraph::V(g);split
igraph::E(g);crossing2(split,g)



cleanEx()
nameEx("dCohen")
### * dCohen

flush(stderr()); flush(stdout())

### Name: dCohen
### Title: Cohen's d
### Aliases: dCohen

### ** Examples

#one-sample
x=c(1:10,5,6,3:8)
dCohen(x,mu0=7)

#two-sample independent
y=1:15
dCohen(x,y)

#two-sample paired
dCohen(x,1:18,paired=TRUE)



cleanEx()
nameEx("dataSet")
### * dataSet

flush(stderr()); flush(stdout())

### Name: dataSet
### Title: Obtain a Dataset from a Formula
### Aliases: dataSet

### ** Examples

dataSet(mpg~.-disp,mtcars,10:20)



cleanEx()
nameEx("get_subgraphs")
### * get_subgraphs

flush(stderr()); flush(stdout())

### Name: get_subgraphs
### Title: Split a Graph into Subgraphs
### Aliases: get_subgraphs

### ** Examples

g=igraph::graph_from_literal(1-2,2-3:4,3-4:5:6,5-1)
split=c("A","A","B","B","A","B")
igraph::V(g);split
igraph::V(get_subgraphs(g,split)[[1]])
igraph::V(get_subgraphs(g,split)[[2]])



cleanEx()
nameEx("graph_attr_from_df")
### * graph_attr_from_df

flush(stderr()); flush(stdout())

### Name: graph_attr_from_df
### Title: Add Graph Attributes to a Graph from a Data Frame
### Aliases: graph_attr_from_df

### ** Examples

g=igraph::graph_from_literal(1-2,2-3:4,3-4:5:6,5-1)
df=data.frame(name="Test Graph",descr="A graph")
graph_attr_from_df(g,df)



cleanEx()
nameEx("impNA")
### * impNA

flush(stderr()); flush(stdout())

### Name: impNA
### Title: Impute Missing Values
### Aliases: impNA

### ** Examples

v1=c(2,5,3,NA,2,4,1,NA)
#Replace values with the mean
impNA(v1,na.rm=TRUE)
#Replace values with the minimum
impNA(v1,min,na.rm=TRUE)



cleanEx()
nameEx("isInt")
### * isInt

flush(stderr()); flush(stdout())

### Name: isInt
### Title: Determine if the Input contains Integers
### Aliases: isInt

### ** Examples

isInt(c(3,3.23,Inf))



cleanEx()
nameEx("laCrossProd")
### * laCrossProd

flush(stderr()); flush(stdout())

### Name: laCrossProd
### Title: Cross Product (Linear Algebra)
### Aliases: laCrossProd

### ** Examples

x=c(1,2,1)
y=1:3
laCrossProd(x,y)



cleanEx()
nameEx("lines3D")
### * lines3D

flush(stderr()); flush(stdout())

### Name: lines3D
### Title: Lines in 3D
### Aliases: lines3D

### ** Examples

pl=coord3D(30)
lines3D(pl,0:10,0:10,rep(0,11))
lines3D(pl,0:10,0:10,c(0,2,1,3:8,7,5),col=2)



cleanEx()
nameEx("lmPartReg")
### * lmPartReg

flush(stderr()); flush(stdout())

### Name: lmPartReg
### Title: Partial Regression Plot
### Aliases: lmPartReg

### ** Examples

lmod=lm(mpg~.,mtcars)
lmPartReg(lmod,"wt")



cleanEx()
nameEx("lmSub")
### * lmSub

flush(stderr()); flush(stdout())

### Name: lmSub
### Title: Best Linear Model in Subset Selection
### Aliases: lmSub

### ** Examples

subs=leaps::regsubsets(mpg~.,mtcars)
summary(lmSub(subs,3))



cleanEx()
nameEx("logistErrorRate")
### * logistErrorRate

flush(stderr()); flush(stdout())

### Name: logistErrorRate
### Title: Calculate the Error Rate and Results Table for Logistic
###   Regression Models
### Aliases: logistErrorRate

### ** Examples

gmod=glm(state~.,binomial,Puromycin)
logistErrorRate(gmod)



cleanEx()
nameEx("outliers")
### * outliers

flush(stderr()); flush(stdout())

### Name: outliers
### Title: Find Outliers
### Aliases: outliers

### ** Examples

x=c(100,30:40,101,25:28)
outliers(x)



cleanEx()
nameEx("plotCol")
### * plotCol

flush(stderr()); flush(stdout())

### Name: plotCol
### Title: Plot Colors
### Aliases: plotCol

### ** Examples

plotCol("maroon")



cleanEx()
nameEx("pop.sd")
### * pop.sd

flush(stderr()); flush(stdout())

### Name: pop.sd
### Title: Calculate the Population Standard Deviation
### Aliases: pop.sd

### ** Examples

pop.sd(c(1:6,NA,7:10))



cleanEx()
nameEx("pop.var")
### * pop.var

flush(stderr()); flush(stdout())

### Name: pop.var
### Title: Calculate the Population Variance
### Aliases: pop.var

### ** Examples

pop.var(c(1:6,NA,7:10))



cleanEx()
nameEx("predict.regsubsets")
### * predict.regsubsets

flush(stderr()); flush(stdout())

### Name: predict.regsubsets
### Title: Obtain Predictions using Subset Selection
### Aliases: predict.regsubsets

### ** Examples

subs=leaps::regsubsets(mpg~.,mtcars,subset=1:25)
predict(subs,3L,mtcars[26:32,])



cleanEx()
nameEx("projMatrix")
### * projMatrix

flush(stderr()); flush(stdout())

### Name: projMatrix
### Title: Create the Projection Matrix of a Matrix
### Aliases: projMatrix

### ** Examples

projMatrix(matrix(c(3,4,-1,2,1,1),3))



cleanEx()
nameEx("qqlineHalf")
### * qqlineHalf

flush(stderr()); flush(stdout())

### Name: qqlineHalf
### Title: Line through a Half-Normal Plot
### Aliases: qqlineHalf

### ** Examples

z=rnorm(100)
faraway::halfnorm(z)
qqlineHalf(z)



cleanEx()
nameEx("rcpp_hello_world")
### * rcpp_hello_world

flush(stderr()); flush(stdout())

### Name: rcpp_hello_world
### Title: Simple function using Rcpp
### Aliases: rcpp_hello_world

### ** Examples

## Not run: 
##D rcpp_hello_world()
## End(Not run)



cleanEx()
nameEx("round2")
### * round2

flush(stderr()); flush(stdout())

### Name: round2
### Title: Round to the Nearest Number
### Aliases: round2

### ** Examples

round2(2.5)



cleanEx()
nameEx("smallstuff-package")
### * smallstuff-package

flush(stderr()); flush(stdout())

### Name: smallstuff-package
### Title: A short title line describing what the package does
### Aliases: smallstuff-package smallstuff
### Keywords: package

### ** Examples

  ## Not run: 
##D      ## Optional simple examples of the most important functions
##D      ## These can be in \dontrun{} and \donttest{} blocks.   
##D   
## End(Not run)



cleanEx()
nameEx("span3D")
### * span3D

flush(stderr()); flush(stdout())

### Name: span3D
### Title: Span of a Matrix
### Aliases: span3D

### ** Examples

span3D(matrix(c(1,0,0,1,1,1),3))



cleanEx()
nameEx("systemEq")
### * systemEq

flush(stderr()); flush(stdout())

### Name: systemEq
### Title: Solve a System of Equations
### Aliases: systemEq

### ** Examples

systemEq(matrix(c(1:3,2,4,4),3),c(3,6,7))



cleanEx()
nameEx("vector2D")
### * vector2D

flush(stderr()); flush(stdout())

### Name: vector2D
### Title: Add a Vector to a 2D Coordinate System
### Aliases: vector2D

### ** Examples

a=c(2,4)
b=c(0,3)
coord2D()
vector2D(a)
vector2D(b)
vector2D(a-b,b,"blue")



cleanEx()
nameEx("vector3D")
### * vector3D

flush(stderr()); flush(stdout())

### Name: vector3D
### Title: Add a Vector to a 3D Coordinate System
### Aliases: vector3D

### ** Examples

a=c(2,4,8)
b=c(6,0,4)
pl=coord3D()
vector3D(pl,a)
vector3D(pl,b)
vector3D(pl,a-b,b,3)



cleanEx()
nameEx("weight_distribution")
### * weight_distribution

flush(stderr()); flush(stdout())

### Name: weight_distribution
### Title: Weight Distribution of a Graph
### Aliases: weight_distribution

### ** Examples

g=igraph::graph_from_literal(1-2,2-3:4,3-4:5:6,5-1)
igraph::E(g)$weight=c(1,2,1,4,2,1,1)
table(igraph::strength(g))/6
weight_distribution(g)



cleanEx()
nameEx("withinPC")
### * withinPC

flush(stderr()); flush(stdout())

### Name: withinPC
### Title: Calculate Row or Column Percentages
### Aliases: withinPC

### ** Examples

(X=matrix(c(1:12),3))
withinPC(X)



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
