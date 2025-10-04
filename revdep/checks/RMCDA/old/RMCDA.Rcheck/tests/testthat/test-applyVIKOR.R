library(testthat)

test_that("apply.VIKOR works for provided example and returns the expected structure", {

  A <- matrix(c(250, 200, 300, 275, 225, 16, 16, 32, 32, 16, 12, 8, 16, 8, 16, 5, 3, 4, 4, 2), nrow=5, ncol=4)
  colnames(A)<-c("Price", "Memory", "Camera", "Looks")
  rownames(A)<-paste0("Mobile ", seq(1, 5, 1))
  A[,"Price"] <- -A[,"Price"]
  results <- apply.VIKOR(A, c(0.35, 0.3, 0.2, 0.15))
  #'
  expect_type(results, "list")
  expect_length(results[[1]], nrow(A))
  expect_length(results[[2]], nrow(A))
  expect_type(results[[1]], "character")
  expect_type(results[[2]], "double")
})
