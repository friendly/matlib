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

# In the following example, we use data from Favara and Imbs (2015). 
# The dataset can be downloaded from GitHub:
repo <- "chaisemartinPackages/ApplicationData/main" 
file <- "favara_imbs_did_multiplegt_dyn.dta"
url <- paste("https://raw.githubusercontent.com", repo, file, sep = "/")
favara_imbs <-  haven::read_dta(url)

# Estimating 3 non-normalized event-study effects and two placebo 
# effects of banking deregulations on loans volume:
summary(did_multiplegt_dyn(
    df = favara_imbs,
    outcome = "Dl_vloans_b",
    group = "county",
    time = "year",
    treatment = "inter_bra",
    effects = 2,
    placebo = 1,
    cluster = "state_n",
    graph_off = TRUE
))

# Please note that some of the standard errors displayed above could differ from those 
# reported in de Chaisemartin and D'Haultfoeuille (2020b) due to coverage-improving 
# changes to the variance estimator.

# See the did_multiplegt_dyn GitHub page for further examples and details.



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
