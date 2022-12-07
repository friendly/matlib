pkgname <- "AquaBPsim"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('AquaBPsim')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("avail_selection")
### * avail_selection

flush(stderr()); flush(stdout())

### Name: avail_selection
### Title: Available as selection candidates
### Aliases: avail_selection

### ** Examples




cleanEx()
nameEx("breeding_values")
### * breeding_values

flush(stderr()); flush(stdout())

### Name: breeding_values
### Title: Simulating estimated breeding values
### Aliases: breeding_values

### ** Examples





cleanEx()
nameEx("cor_var")
### * cor_var

flush(stderr()); flush(stdout())

### Name: cor_var
### Title: simulating correlated variable
### Aliases: cor_var

### ** Examples


cor_var(c(2,4,2,2,6,7,5,6,6,7,9,4,5), 0.5)



cleanEx()
nameEx("deltaG_F")
### * deltaG_F

flush(stderr()); flush(stdout())

### Name: deltaG_F
### Title: Calculating genetic gain and rate of inbreeding
### Aliases: deltaG_F

### ** Examples

## Not run: 
##D deltaG_F()
## End(Not run)



cleanEx()
nameEx("founderpopfam")
### * founderpopfam

flush(stderr()); flush(stdout())

### Name: founderpopfam
### Title: Founder population family design
### Aliases: founderpopfam

### ** Examples


ped <- founderpopfam(Nm=60, Nm2=0,
                     Nf=60, Nf2=0,
                     batch = c(0,1,2),
                     Ntraits=2,
                     TraitsIndex = 2,
                     Rgen= matrix(c(1.00   , 0.48,
                                    0.48   , 1.00),
                                 nrow = 2),
                     Rcom= matrix(c(1.00  , 0.5,
                                    0.5   , 1.00),
                                 nrow = 2),
                     Rres= matrix(c(1.00   , 0.32,
                                    0.32   , 1.00),
                                 nrow = 2),
                     mean=c(50,500),
                     a_var=c(200,8000),
                     c_var=c(150,1000),
                     e_var= c(250,12000))

              
ped <- founderpopfam(Nm=60, 
                     Nf=60, 
                     Nm2=120,
                     Nf2=120,
                     Nbatch = 4,
                     batch2 = c(-3,-2,-1,0),
                     Ntraits=2,
                     TraitsIndex = c(1,2),
                     Rgen= matrix(c(1.00   , 0.48,
                                    0.48   , 1.00),
                                 nrow = 2),
                     Rcom= matrix(c(1.00  , 0,
                                    0   , 1.00),
                                 nrow = 2),
                     Rres= matrix(c(1.00   , 0.32,
                                    0.32   , 1.00),
                                 nrow = 2),
                     mean=c(50,500),
                     a_var=c(200,8000),
                     c_var=c(0,0),
                     e_var= c(250,12000),
                     est_EBV = TRUE,
                     EBV= c("pheno", "EBV"),
                     accuracy= c(NA,0.78),
                     indexweight= c(1,5))




cleanEx()
nameEx("founderpopgroup")
### * founderpopgroup

flush(stderr()); flush(stdout())

### Name: founderpopgroup
### Title: Founder population group mating design
### Aliases: founderpopgroup

### ** Examples

ped <- founderpopgroup(Nm=60, Nm2=0,
                     Nf=60, Nf2 = 0,
                     batch = c(0,1,2),
                     Ntraits=2,
                     TraitsIndex = 2,
                     Rgen= matrix(c(1.00   , 0.48,
                                    0.48   , 1.00),
                                 nrow = 2),
                     Rcom= matrix(c(1.00  , 0,
                                    0   , 1.00),
                                 nrow = 2),
                     Rres= matrix(c(1.00   , 0.32,
                                    0.32   , 1.00),
                                 nrow = 2),
                     mean=c(50,500),
                     a_var=c(200,8000),
                     c_var=c(0,0),
                     e_var= c(250,12000))

              
ped <- founderpopgroup(Nm=60,
                     Nf=60,
                     Nm2=120,
                     Nf2=120,
                     Nbatch = 4,
                     batch2 = c(-3,-2,-1,0),
                     Ntraits=2,
                     TraitsIndex = c(1,2),
                     Rgen= matrix(c(1.00   , 0.48,
                                    0.48   , 1.00),
                                 nrow = 2),
                     Rcom= matrix(c(1.00  , 0,
                                    0   , 1.00),
                                 nrow = 2),
                     Rres= matrix(c(1.00   , 0.32,
                                    0.32   , 1.00),
                                 nrow = 2),
                     mean=c(50,500),
                     a_var=c(200,8000),
                     c_var=c(0,0),
                     e_var= c(250,12000),
                     est_EBV = TRUE,
                     EBV= c("pheno", "EBV"),
                     accuracy= c(NA,0.78),
                     indexweight= c(1,5))




cleanEx()
nameEx("gen_param")
### * gen_param

flush(stderr()); flush(stdout())

### Name: gen_param
### Title: Importing genetic parameters from excel file
### Aliases: gen_param

### ** Examples

## Not run: 
##D BPdata <- gen_param("example.xlsx")
## End(Not run)



cleanEx()
nameEx("groupmating")
### * groupmating

flush(stderr()); flush(stdout())

### Name: groupmating
### Title: Group mating
### Aliases: groupmating

### ** Examples


{ped <- founderpopgroup(Nm=60,
                     Nf=60,
                     Nm2=120,
                     Nf2=120,
                     Nbatch = 4,
                     batch2 = c(-3,-2,-1,0),
                     Ntraits=2,
                     TraitsIndex = 2,
                     Rgen= matrix(c(1.00   , 0.48,
                                    0.48   , 1.00),
                                 nrow = 2),
                     Rcom= matrix(c(1.00  , 0,
                                    0   , 1.00),
                                 nrow = 2),
                     Rres= matrix(c(1.00   , 0.32,
                                    0.32   , 1.00),
                                 nrow = 2),
                     mean=c(50,500),
                     a_var=c(200,8000),
                     c_var=c(0,0),
                     e_var= c(250,12000))
                     
 Mating <- groupmating(gen = 0,
                       batch=-3,
                       No=1000,
                       contr_m = 0.5,
                       contr_f = 0.5)
}



cleanEx()
nameEx("offspringFSfam")
### * offspringFSfam

flush(stderr()); flush(stdout())

### Name: offspringFSfam
### Title: Creating offspring for family design
### Aliases: offspringFSfam

### ** Examples




cleanEx()
nameEx("offspringFSgroup")
### * offspringFSgroup

flush(stderr()); flush(stdout())

### Name: offspringFSgroup
### Title: Creating offspring for a group mating design
### Aliases: offspringFSgroup

### ** Examples





cleanEx()
nameEx("preselphen")
### * preselphen

flush(stderr()); flush(stdout())

### Name: preselphen
### Title: Preselecting offspring based on phenotype
### Aliases: preselphen

### ** Examples


                  
                  



cleanEx()
nameEx("preselrandom")
### * preselrandom

flush(stderr()); flush(stdout())

### Name: preselrandom
### Title: Randomly preselecting offspring
### Aliases: preselrandom

### ** Examples




cleanEx()
nameEx("preselselcand")
### * preselselcand

flush(stderr()); flush(stdout())

### Name: preselselcand
### Title: Preselection of selection candidates
### Aliases: preselselcand

### ** Examples




cleanEx()
nameEx("randommating")
### * randommating

flush(stderr()); flush(stdout())

### Name: randommating
### Title: Random mating family design
### Aliases: randommating

### ** Examples

{ ped <- founderpopfam(Nm=60,
                     Nf=60,
                     Nm2=0,
                     Nf2=0,
                     Ntraits=2,
                     TraitsIndex = 2,
                     Rgen= matrix(c(1.00   , 0.48,
                                    0.48   , 1.00),
                                 nrow = 2),
                     Rcom= matrix(c(1.00  , 0.5,
                                    0.5   , 1.00),
                                 nrow = 2),
                     Rres= matrix(c(1.00   , 0.32,
                                    0.32   , 1.00),
                                 nrow = 2),
                     mean=c(50,500),
                     a_var=c(200,8000),
                     c_var=c(150,1000),
                     e_var= c(250,12000))
                     
Mating <- randommating(gen = 0,
                       Nfam_FS = 120)
}



cleanEx()
nameEx("select")
### * select

flush(stderr()); flush(stdout())

### Name: select
### Title: Selection
### Aliases: select

### ** Examples




cleanEx()
nameEx("survive")
### * survive

flush(stderr()); flush(stdout())

### Name: survive
### Title: Survive
### Aliases: survive

### ** Examples




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
