library(testthat)
library(matrixStats)

test_that("apply.WSM_WPM works for provided example and returns the expected structure", {

  mat <- matrix(c(250, 200, 300, 275, 225, 16, 16, 32, 32, 16, 12, 8, 16, 8, 16, 5, 3, 4, 4, 2), nrow=5, ncol=4)
  colnames(mat)<-c("Price", "Storage space", "Camera", "Looks")
  rownames(mat)<-paste0("Mobile ", seq(1, 5, 1))
  beneficial.vector <- c(2, 3, 4)
  weights <- c(0.25, 0.25, 0.25, 0.25)
  result <- apply.WSM_WPM(mat, beneficial.vector, weights, "WSM")

  expect_type(result, "double")

  expect_length(result, nrow(mat))

})
