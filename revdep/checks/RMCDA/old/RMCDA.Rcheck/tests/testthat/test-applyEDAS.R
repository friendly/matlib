library(testthat)

test_that("apply.EDAS works for provided example and returns the expected structure", {

  mat <- matrix(c(250, 200, 300, 275, 225,
    16, 16, 32, 32, 16,
    12, 8, 16, 8, 16,
    5, 3, 4, 4, 2), nrow=5)
  colnames(mat)<-c("Price/cost", "Storage Space", "Camera", "Looks")
  rownames(mat)<-paste0("Mobile", 1:5)

  mat[,"Price/cost"]<--mat[,"Price/cost"]
  weights <- c(0.35, 0.25, 0.25, 0.15)

  result <- apply.EDAS(mat, weights)
  expect_length(result, nrow(mat))
  expect_type(result, "double")

})
