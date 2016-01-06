## Test environments
* local Windows 7 install, R 3.1.2
* win-builder (R-release 3.2.3 (2015-12-10), R-devel (2016-01-06 r69875))

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs

## Comments
This is a major release, offering the following new functionality and correcting some documentation
problems.

- use `corner()` in vectors3d.Rd
- added `arc()` for 2D, 3D vector diagrams to show angles between vectors
- added `regvec3d()` for 2D, 3D vector diagrams representing a bivariate multiple regression model, `lm(y ~ x1 + x2)` [thx: John Fox]
- added internal `.arrows()` to produce nice arrows in 2D `vector()` diagrams
- fixed numerous small problems in vector diagrams
- fixed some erroneous statements in vignettes
- `showEqn()` now aligns terms vertically and prints without quotes
- reversed sense of `absolute` in `points_on_line()` and clarified documentation


