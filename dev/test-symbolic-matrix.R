# test symbolicMatrix

source(here::here("dev", "symbolicMatrix.R"))

symbolicMatrix()
symbolicMatrix("\\beta")
symbolicMatrix(nrow="n", ncol="n")
symbolicMatrix(ncol=3)
symbolicMatrix(nrow=4)
symbolicMatrix(nrow=4, ncol=4)
symbolicMatrix(nrow=1)
symbolicMatrix(ncol=1)
symbolicMatrix(nrow=1, ncol=4)
symbolicMatrix(nrow=3, ncol=1)

symbolicMatrix(nrow=3, ncol=3, diag=TRUE)
symbolicMatrix(nrow="n", ncol="n", diag=TRUE)

