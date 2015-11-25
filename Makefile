PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: install

build:
	cd ..;\
	R CMD build $(PKGSRC)

install:
	cd ..;\
	R CMD INSTALL $(PKGSRC)

check:
	Rscript -e "devtools::check(document = FALSE, args = '--as-cran')"

news:
	sed -e 's/^-/  -/' -e 's/^## *//' -e 's/^# //' <NEWS.md | fmt -80 >NEWS

vignettes:
	Rscript -e "setwd('vignettes');library('knitr');files=dir("*.Rmd");for(file in files) knit2html(file)"
#	mv -f source/*.html html/

#test:
#	Rscript -e "library('testthat',quietly=TRUE);library('matlib',quietly=TRUE);options(warn=2);test_dir('tests/tests')"

clean:
#	$(RM) src/*.o
#	$(RM) src/*.so
	$(RM) ../$(PKGNAME)_$(PKGVERS).tar.gz
	$(RM) -r ../$(PKGNAME).Rcheck/

