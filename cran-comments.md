## Test environments
* local Windows 7 install, 3.6.3 (2020-02-29)
* travis.ci on github
* win-builder R version 4.0.0 alpha (2020-03-26 r78078)
* win-builder R release version 3.6.3 (2020-02-29)

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs

## Comments
This is a minor release, fixing a bug in `gaussianElimination`(#28) 


# matlib 0.9.3
- fix edge-case bug in `gaussianElimination` [Thx: JanaJarecki]
- improve `plotEqn` to handle special cases

# matlib 0.9.2

- Bug fix in arrows3d.r [Thx: Douglas Whitaker]
- Bump package version
- matlib gets a hex sticker
- Incorporated a numerical tolerance in `GramSchmidt()` [John Fox]
- Improved computation of SVD [John Fox]
- now export `printMatrix()`
- now export `circle3d()` [req: Marco Scazzocchio]
- now use_revdep()

## Reverse dependencies

- *Wow, no problems at all. :)*
