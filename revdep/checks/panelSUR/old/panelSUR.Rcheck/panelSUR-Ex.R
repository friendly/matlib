pkgname <- "panelSUR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('panelSUR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("SURest")
### * SURest

flush(stderr()); flush(stdout())

### Name: SURest
### Title: EC SUR System Models Estimation on (Unbalanced) Panel Data
### Aliases: SURest

### ** Examples

data("SURdata", package="panelSUR")

## Data preparation
library(plm)
datap <- pdata.frame(data, index=c("IND", "TIME"))

## Equations specification
eq1<-Y1~X1+X2
eq2<-Y2~X1+X2+X3
eq3<-Y3~X2+X3
eqlist<-c(eq1,eq2,eq3)

## Constraints specification
constraints<-c("eq1$X2=eq2$X1","eq2$X3=eq3$X2")

## System estimation
mod1<-SURest(eqlist=eqlist,restrictions=constraints,method="2wayQUE",data=datap)



cleanEx()
nameEx("printSUR")
### * printSUR

flush(stderr()); flush(stdout())

### Name: printSUR
### Title: Print summary of estimated equation system
### Aliases: printSUR
### Keywords: models

### ** Examples

data("SURdata", package="panelSUR")

## Data preparation
library(plm)
datap <- pdata.frame(data, index=c("IND", "TIME"))

## Equations specification
eq1<-Y1~X1+X2
eq2<-Y2~X1+X2+X3
eqlist<-c(eq1,eq2)

## System estimation
mod1<-SURest(eqlist=eqlist,method="1wayWB",data=datap)

## Summary of estimation results
printSUR(mod1)



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
