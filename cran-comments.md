## Test environments
* local Windows 7 install, R 3.1.2
* win-builder (R version 3.2.3 Patched (2016-02-04 r70085), R-devel (2016-02-06 r70117))

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs

## Comments
This is a patch release, fixing a problem reported, "does not work with R < 3.2.0"

# matlib 0.7.2

- added argument `error.sphere` to `plot.regvec3d()` [JF]
- remove use of `lengths()` in `corner()` to avoid R version dependency


