# illustrating contrasts

library(matlib)

levels <- letters[1:4]
C <- contr.treatment(levels) |> print()

mu <- matrix(paste0("\\mu_", letters[1:4]), 4, 1)

X <- rbind(Avg= 1/4, t(C))
X

# I ran into problems with the display of row and column names which is TRUE by default when they exist,
# so set this FALSE
options(print.latexMatrix = list(display.labels=FALSE))

matX <- latexMatrix(X, fractions = TRUE)



Eqn(matX,  "\\times", latexMatrix(mu), " = ",
    matX %*% latexMatrix(mu))

# Gives an error:

# Error in `Dimnames<-.latexMatrix`(`*tmp*`, value = dimnames) : 
#   there should be 5 row names
# Traceback:
# 7. stop("there should be ", nrow, " row names") at latexMatrix.R#1063
# 6. `Dimnames<-.latexMatrix`(`*tmp*`, value = dimnames) at latexMatrix.R#1042
# 5. `Dimnames<-`(`*tmp*`, value = dimnames) at latexMatrixOperations.R#416
# 4. matmult.latexMatrix(x, y) at latexMatrixOperations.R#355
# 3. matmult(x, y) at latexMatrixOperations.R#424
# 2. `%*%.latexMatrix`(matX, latexMatrix(mu)) at Eqn.R#200
# 1. Eqn(matX, "\\times", latexMatrix(mu), " = ", matX %*% latexMatrix(mu))


# Warning message:
#   In file.remove(tmpfile) :
#   cannot remove file 'C:\Users\friendly\AppData\Local\Temp\RtmpIzvCX9\file749469bf76d2', reason 'Permission denied'

# Then, something weird happens --- console no longer displays results

# then, the console no longer displays results