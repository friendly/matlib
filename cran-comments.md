## Test environments
* local Windows 7 install, R 3.1.2
* win-builder (devel)

## R CMD check results
There were no ERRORs or WARNINGs

## NOTE
There were no NOTEs

## Comments
This is a minor release, offering the following new functionality

- added `len()` convenience function for Euclidean lengths
- added `plotEqn3d()` function using rgl to plot equations in 3 unknowns
- reorganized `matlib.R` to become a package .Rd document
- renamed `proj()` -> `Proj()` to avoid conflict with `stats::proj()`
- added `vectors()` for plotting geometric diagrams
- added vignettes/linear-equations


