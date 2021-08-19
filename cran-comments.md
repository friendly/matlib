## Test environments
* local Windows 10 install, 4.0.2 (2020-06-22)
* travis.ci on github
* win-builder R Under development (unstable) (2021-08-13 r80752)

## Comments
This is a minor release, designed to fix the problem 
'CRAN packages requiring webshot2 but not declaring it'

## R CMD check results
There were no ERRORs or WARNINGs.  The proposed fix generates
the following note, which I'm told is to be expected.

Suggests or Enhances not in mainstream repositories:
  webshot2
Availability using Additional_repositories specification:
  webshot2   yes   https://dmurdoch.github.io/drat
* checking package namespace information ... OK
* checking package dependencies ... NOTE
Package suggested but not available for checking: 'webshot2'


# matlib 0.9.5

- added `xprod()` for vector cross-product
- added data-beta vignette
- fix `sprintf` warning from LU [Phil Chalmers]
- better plotting of planes in plotEqn3d, alpha=0.9


## Reverse dependencies

- *Wow, no problems at all. :)*
