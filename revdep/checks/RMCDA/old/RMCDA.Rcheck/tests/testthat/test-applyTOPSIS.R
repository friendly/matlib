library(testthat)

test_that("apply.TOPSIS works for provided example and returns the expected structure", {

  A <- matrix(c(250, 200, 300, 275, 225, 16, 16, 32, 32, 16, 12, 8, 16, 8, 16, 5, 3, 4, 4, 2), nrow=5, ncol=4)
  colnames(A)<-c("Price", "Storage space", "Camera", "Looks")
  rownames(A)<-paste0("Mobile ", seq(1, 5, 1))
  A[,"Price"] <- -A[,"Price"]
  results <- apply.TOPSIS(A, c(1/4, 1/4, 1/4, 1/4))
  expect_length(results, nrow(A))

  expect_type(results, "double")
})
