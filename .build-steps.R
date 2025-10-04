# keep rgl from popping up windows
Sys.setenv(RGL_USE_NULL = TRUE)


# args = c('--resave-data','--compact-vignettes=both')
devtools::check()
devtools::build()


# then, test with win builder
devtools::check_win_devel()

library(revdepcheck)
revdep_reset()
revdep_check(num_workers = 4)


# submit to cran
devtools::release()


# NB: building vignettes, then compacting
devtools::build_vignettes()


# build the package site
library(pkgdown)
pkgdown::build_site()
