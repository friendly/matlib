## Test environments
* local Windows 7 install, R 3.1.2
* win-builder (R version 3.3.0 (2016-05-03), R-devel (unstable) (2016-06-03 r70706)

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs

## Comments
This is a mid-size release, improving some functions and adding one new visualization method

# matlib 0.7.3

- Changed gaussianElimination() by defining local ERO functions to make the algorithm clearer; in verbose mode, show each ERO.
- Added a draw argument to `vectors3d()` and `arrows3d()`, which defaults to TRUE. If FALSE, just returns
  returns the "reg.length" to help in scaling.
- Small cosmetic changes to regvec3d().
- Added a `showEig` function to draw eigenvectors superimposed on a dataEllipse [MF]


