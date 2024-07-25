# keep rgl from popping up windows
Sys.setenv(RGL_USE_NULL = TRUE)


# args = c('--resave-data','--compact-vignettes=both')
devtools::check()
devtools::build()


# then, test with win builder
devtools::check_win_devel()

# submit to cran
devtools::release()


# NB: building vignettes, then compacting
devtools::build_vignettes()

# no longer necessary
tools::compactPDF("doc/HE-examples.pdf", gs_quality="ebook")
# compacted ‘HE-examples.pdf’ from 765Kb to 415Kb

# build the package site
library(pkgdown)
pkgdown::build_site()
