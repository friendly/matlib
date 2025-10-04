library(testthat)
library(matlib)
test_that("apply.DEMATEL works for provided example and returns the expected structure", {

  comparisons.mat <- matrix(c(0, 3, 3, 4,
   1, 0, 2, 1,
   1, 2, 0, 2,
   1, 2, 1, 0), nrow=4)
  rownames(comparisons.mat)<-c("Price/cost", "Storage Space", "Camera", "Processor")
  colnames(comparisons.mat)<-c("Price/cost", "Storage Space", "Camera", "Processor")
  result <- apply.DEMATEL(comparisons.mat)
  expect_length(result[[1]], ncol(comparisons.mat))
  expect_length(result[[2]], ncol(comparisons.mat))
  expect_type(result[[1]], "double")
  expect_type(result[[2]], "double")

})
