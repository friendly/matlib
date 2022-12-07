## Test environments
* local Windows 10 install, 4.1.3 (2022-03-10)
* local windows 7, 4.2.1 (2022-06-23 ucrt)
* travis.ci on github
* win-builder R Under development (unstable) (2022-12-05 r83406 ucrt)

## Comments
This is a minor release, designed to fix a few bugs 

## R CMD check results
There were no ERRORs or WARNINGs.  


# matlib 0.9.6

- fix some subtle problems related to the use of `MASS::fractions()` in various display functions, #42. (Thx: Phil Chalmers)
- Fix a problem with `showEqn(..., simplify=TRUE), #45. (Thx: John Fox)
- Remove Suggests: rglwidget as this is now in the rgl package (Thx: Duncan Murdoch);  Merge pull request #39 from dmurdoch/norglwidget 


## Reverse dependencies

> devtools::revdep()
[1] "AquaBPsim"     "biplotbootGUI" "dymo"          "hyperoverlap"  "IOLS"          "MIIPW"        
[7] "smallstuff"    "VIRF"

## revdepcheck results

We checked 9 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 1 packages

Issues with CRAN packages are summarised below.

### Failed to check

* MIIPW (NA)

I have contacted the package maintainer, Atanu Bhattacharjee <atanustat at gmail.com> regarding this problem.


