## Test environments
* local Windows 7 install, 3.5.2 (2018-12-20)
* travis.ci on github
* win-builder R Under development (unstable) (2019-05-25 r76601)

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs

## Comments
This is a minor release, fixing several small problems
and improved computation for GramSchmidt & SVD

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
