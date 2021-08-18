# keep rgl from popping up windows
Sys.setenv(RGL_USE_NULL = TRUE)

# The standard R Studio / devtools build does not compress vignettes. To avoid the warning
# 
# 'gs+qpdf' made some significant size reductions
# 
# it was necessary to build manually, using

#Sys.setenv(R_GSCMD="C:/Program Files/gs/gs9.21/bin/gswin64c.exe")
Sys.setenv(R_GSCMD="C:/Program Files/gs/gs9.53.3/bin/gswin64c.exe")

devtools::build(args = c('--resave-data','--compact-vignettes=both'))

#devtools::build(args = c('--compact-vignettes=both'))

# then, test with win builder
args = c('--resave-data','--compact-vignettes=both')
devtools::check_win_devel(args=args)

# submit to cran
args = c('--resave-data','--compact-vignettes=both')
#devtools::submit_cran(args=args)
devtools::release(args=args)


# NB: building vignettes, then compacting
devtools::build_vignettes()

tools::compactPDF("doc/HE-examples.pdf", gs_quality="ebook")
# compacted ‘HE-examples.pdf’ from 765Kb to 415Kb